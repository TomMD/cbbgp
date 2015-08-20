{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ParallelListComp    #-}

-- |CBBGP: PullBuildBenchGraphPush
--
-- CBBGP is a script for pulling from one or more repositories, building,
-- benchmarking, producing performance graphs, and pushing these results
-- (graphs included) to a repository.
--
-- Use:
--
-- CBBGP is configured by an INI file (yes, old school is back!) in the form:
--
-- @
-- [ProjectName]
-- repo=<git repository URL>
-- builder=<shell command to build the code, for example `make`, ./install.sh, or stack install>
-- tagger=<shell command to get the tag with which the result will be labeled>
-- resultRepo=<git repository URL for the result>
-- freeform1=<command to be benchmarked>
-- freeform2=<command to be benchmarked>
-- freeform3=<command to be benchmarked>
--
-- [Project-2]
-- ...
-- [Project-N]
-- @

module Main where

import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import qualified Control.Exception as X
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)
import BenchUtils (readProcessMemory)
import System.IO

-- Running commands periodically:
import Data.Time hiding (DiffTime)
import Data.IORef
import Control.Concurrent.Thread.Delay
import System.IO.Temp

-- Interacting with the human:
import Data.Ini
import SimpleGetOpt
import Data.Maybe (isNothing, catMaybes)
import System.Exit

-- Interacting with git:
import System.Process (runInteractiveCommand, waitForProcess, callCommand)
import System.Directory
import System.FilePath ((</>), (<.>), addExtension, takeExtension)

import Git
import Utils
import Graph

argCfg :: OptSpec Opts
argCfg =
  OptSpec { progDefaults    = Opts (24*3600) ""

          , progOptions     =
              [ Option ['p'] ["period"] "Number of seconds between each run"
                  $ ReqArg "seconds" $ \a s ->
                        case (readMaybe a :: Maybe Integer) of
                          Just n | n > 0 -> Right s { period = fromIntegral n }
                          _              -> Left "Invalid period."
              ]
          , progParamDocs   = [ ("Configuration", "An INI configuration file") ]
          , progParams      = \p s -> Right (s { iniFile = p })
          }

type Config = HashMap Text BenchInfo
data BenchInfo = BI { srcRepo       :: Text
                    , resultRepo    :: Text
                    , buildCommand  :: Text
                    , tagCommand    :: Text
                    , benchmarks    :: [Benchmark]
                    }

data Benchmark = BM { name, benchCommand :: Text }

data Opts = Opts { period  :: NominalDiffTime
                 , iniFile :: FilePath
                 }

kwRepo,kwResultRepo,kwBuilder :: Text
kwRepo       = "repo"
kwResultRepo = "resultRepo"
kwBuilder    = "builder"
kwTagger     = "tagger"

reservedWords :: [Text]
reservedWords = [kwRepo,kwResultRepo,kwBuilder,kwTagger]

main :: IO ()
main = do
    opts <- getOpts argCfg
    mcfg <- readConfig (iniFile opts)
    case mcfg of
        Left e    -> print e >> dumpUsage argCfg >> exitFailure
        Right cfg ->
            do putStrLn $ "Read configuration file.  Executing CBBGP for " ++ show (HashMap.size cfg) ++ " projects."
               periodically (period opts) (runBenchs cfg)

-- With a given period, execute the IO action... forever.
periodically :: NominalDiffTime -> IO () -> IO ()
periodically period run =
  do nextTime <- newIORef . addUTCTime period =<< getCurrentTime
     forever $ X.catch (go nextTime) (\(_ :: X.SomeException) -> return ())
 where
   go nextTimeRef =
    do -- Run
       X.catch run (\(e :: X.SomeException) -> hPutStrLn stderr (show e)  >> return ())
       -- Delay
       start <- getCurrentTime
       delayNominalTime . (`diffUTCTime` start) =<< readIORef nextTimeRef
       -- Set next time window
       writeIORef nextTimeRef . addUTCTime period =<< getCurrentTime
   delayNominalTime :: NominalDiffTime -> IO ()
   delayNominalTime = delay . floor . (*10^6) . realToFrac

-- Read an INI configuration file and return either Just a well-formed Ini
-- with all required fields or Nothing
readConfig :: FilePath -> IO (Either String Config)
readConfig fp =
  do ini <- safeReadIniFile fp
     case ini of
         Left  e -> return $ Left e
         Right c -> return $ maybe (Left "Missing required field in INI.") Right (mapM convert (unIni c))
  where
  convert :: HashMap Text Text -> Maybe BenchInfo
  convert m =
    do srcRepo      <- m ! "repo"
       resultRepo   <- m ! "resultRepo"
       buildCommand <- m ! "builder"
       let tagCommand = HashMap.lookupDefault "echo ''" "tagger" m
       let benchmarks = Prelude.map (uncurry BM) $ Prelude.filter ((`notElem` reservedWords) . fst) (HashMap.toList m)
       return BI {..}

  (!) :: HashMap Text Text -> Text -> Maybe Text
  (!) = flip HashMap.lookup

safeReadIniFile :: FilePath -> IO (Either String Ini)
safeReadIniFile fp = X.catch (readIniFile fp) (\(e :: X.SomeException) -> return $ Left (show e))

-- Given an Ini that contains all required field, make a temporary
-- directory and for each project:
--      - C: git clone
--      - B: build
--      - B: benchmark
--      - G: graph
--      - P: git push
runBenchs :: Config -> IO ()
runBenchs cfg = forM_ (HashMap.keys cfg) (\k -> cbbgp k (cfg HashMap.! k))
 where
  -- For each project, build and run the relevant set of benchmarks.
  cbbgp :: Text -> BenchInfo -> IO ()
  cbbgp (Text.unpack -> project) (BI {..}) =
   withSystemTempDirectory project $ \tmp ->
    do putStrLn $ "Benchmarking " ++ project ++ "..."
       setCurrentDirectory tmp
       srcDir      <- gitClone srcRepo project
       putStrLn      "\tCloned source repository"
       resultDir   <- gitClone resultRepo (project ++ "-results")
       putStrLn      "\tCloned result repository"
       buildResult <- doBuild buildCommand srcDir
       putStrLn      "\tDone building"
       numbers     <- doBench benchmarks srcDir
       putStrLn      "\tDone running"
       dayHour     <- formatTime defaultTimeLocale "%F::%T" <$> getCurrentTime
       let newResultsName = project </> dayHour <.> "raw.results"
       createDirectoryIfMissing True (resultDir </> project)
       tag        <- getTag tagCommand project
       writeFile (resultDir </> newResultsName) (show (tag,numbers))
       gitAddCommit resultDir newResultsName
       gitPush resultDir
       putStrLn      "\tPushed raw results"
       X.catch (do graphFiles <- doGraph resultDir project
                   putStrLn      "\tGraphed results"
                   mapM_ (gitAddCommit resultDir) graphFiles
                   gitPush resultDir)
               (\(_ :: X.SomeException) -> return ())
               -- Sometimes gnuplot produces an identical file and commit
               -- then fails.  We don't care.
       putStrLn      "\tPushed graphs - complete!"

-- Build the project, returning an error or unit.
doBuild :: Text -> FilePath -> IO ()
doBuild cmd dir = withDirectory dir $ callCommand (Text.unpack cmd)

getTag :: Text -> String -> IO String
getTag cmd project = withDirectory project $ do
    (_sin,sout,_serr,ph) <- runInteractiveCommand (Text.unpack cmd)
    tag <- filter (`notElem` badChar) <$> hGetContents sout
    _ec <- waitForProcess ph
    return tag
 where
     badChar :: String
     badChar = "\n\r\DEL"

-- Perform the set of benchmarks and create a new CSV file for the results.
-- Each benchmark has a name and a command.
-- 1) Execute the command, vi readProcessMemory, and record the process
-- time along with memory for each of these benchmarks
doBench :: [Benchmark] -> FilePath -> IO (HashMap Text (Double, DiffTime))
doBench bs dir = withDirectory dir (HashMap.fromList <$> mapM runOne bs)
 where

  runOne :: Benchmark -> IO (Text, (Double, DiffTime))
  runOne (BM name cmd) = do
     putStrLn $ "\trunning " ++ Text.unpack name
     let (c:args) = words (Text.unpack cmd)
     (r,time) <- timeIt $ readProcessMemory c args ""
     return (name, (getMem r, time))

  getMem :: (a, [Double]) -> Double
  getMem (_,ms) = maximum (0:ms)

  timeIt :: IO a -> IO (a, DiffTime)
  timeIt oper =
    do s <- getCurrentTime
       r <- oper
       e <- getCurrentTime
       return (r,realToFrac $ diffUTCTime e s)
