{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

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
import Data.List (sort)
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
import Graphics.Gnuplot.Simple (plotLists, Attribute(..))

-- Interacting with git:
import System.Process (callCommand)
import System.Directory
import System.FilePath ((</>), (<.>), addExtension, takeExtension)

argCfg :: OptSpec Opts
argCfg =
  OptSpec { progDefaults    = Opts (24*3600) ""

          , progOptions     =
              [ Option ['p'] ["period"] "Number of seconds between each run"
                  $ ReqArg "microseconds" $ \a s ->
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

type DiffTime = Double

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
        Nothing  -> dumpUsage argCfg >> exitFailure
        Just cfg ->
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
readConfig :: FilePath -> IO (Maybe Config)
readConfig fp =
  do ini <- safeReadIniFile fp
     case ini of
         Left _  -> return Nothing
         Right c -> return $ mapM convert (unIni c)
  where
  convert :: HashMap Text Text -> Maybe BenchInfo
  convert m =
    do srcRepo      <- m ! "repo"
       resultRepo   <- m ! "resultRepo"
       buildCommand <- m ! "builder"
       tagCommand   <- m ! "tagger" >>= maybe (Just "echo ''") Just
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
       dayHour     <- formatTime defaultTimeLocale "%F::%R" <$> getCurrentTime
       let newResultsName = project </> dayHour <.> "raw.results"
       createDirectoryIfMissing True (resultDir </> project)
       tag        <- getTag tagCommand
       writeFile (resultDir </> newResultsName) (show (tag,numbers))
       graphFiles <- doGraph resultDir project
       putStrLn      "\tGraphed results"
       gitAddCommit resultDir newResultsName
       mapM_ (gitAddCommit resultDir) graphFiles
       putStrLn      "\tCommited files"
       gitPush resultDir
       putStrLn      "\tPushed - complete!"

-- Build the project, returning an error or unit.
doBuild :: Text -> FilePath -> IO ()
doBuild cmd dir = withDirectory dir $ callCommand (Text.unpack cmd)

getTag :: Text -> IO String
getTag cmd = withDirectory project $ do
    (_sin,sout,_serr,ph) <- runInteractiveCommand cmd
    tag <- hGetContents sout
    _ec <- waitForProcess ph
    return tag

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

-- Read in any old results and graph all results together, replacing any
-- current "$benchmarkName.svg" file in the directory ($repo/$project/$name.{time,memory}.svg).
--
-- Pre-existing files should be $repo/$project/*.raw.results and be readable via
-- @read :: String -> HashMap Text (Double, DiffTime)@ (i.e. produced via 'show').
doGraph :: FilePath -> String -> IO [FilePath]
doGraph repoDir projectName =
  do allNumbers <- readOldResults
     let graphs = extractGraphData allNumbers
     concat <$> mapM writeGraph graphs
 where
    extractGraphData :: [HashMap Text (Double, DiffTime)] -> [(Text, [(Double,DiffTime)])]
    extractGraphData ds =
        let allKeys = HashMap.keys (HashMap.unions ds)
        in [(k, catMaybes (map (HashMap.lookup k) ds)) | k <- allKeys]

    readOldResults :: IO [HashMap Text (Double, DiffTime)]
    readOldResults =
      do let dir = repoDir </> projectName
             isResult = (".results" ==) . takeExtension
         resultFiles <- (sort . filter isResult) <$> getDirectoryContents dir
         mapM (fmap read . readFile . (dir </>)) resultFiles

    writeGraph :: (Text, [(Double,DiffTime)]) -> IO [FilePath]
    writeGraph (benchName, mts) = do
        let (ms,ts) = unzip mts
            plot fp xs = plotLists [Custom "terminal" ["svg"], Custom "output" [quote fp]] [xs]
            fpMS = repoDir </> projectName </> Text.unpack benchName <.> "memory" <.> "svg"
            fpTS = repoDir </> projectName </> Text.unpack benchName <.> "time"   <.> "svg"
        plot fpMS ms
        plot fpTS ts
        return [fpMS,fpTS]

    quote :: String -> String
    quote x = concat ["\"",x,"\""]

withDirectory :: FilePath -> IO a -> IO a
withDirectory fp oper =
  do p <- getCurrentDirectory
     setCurrentDirectory fp
     X.finally oper (setCurrentDirectory p)

timeIt :: IO a -> IO (a, DiffTime)
timeIt oper =
  do s <- getCurrentTime
     r <- oper
     e <- getCurrentTime
     return (r,realToFrac $ diffUTCTime e s)

--------------------------------------------------------------------------------
--  Hacky hacky git interface, somewhat non-abstract
--  as it is tied to this use case.
--------------------------------------------------------------------------------

-- Clones a given url into a child directory of the cwd.
gitClone :: Text -> FilePath -> IO FilePath
gitClone url dir =
    do cwd <- getCurrentDirectory
       callCommand $ "git clone " ++ Text.unpack url ++ " " ++ dir
       return (cwd </> dir)

gitAddCommit :: FilePath -> FilePath -> IO ()
gitAddCommit repoDir file = withDirectory repoDir $ do
    callCommand $ "git add " ++ file
    callCommand $ "git commit -m 'auto-commit from mss-bench' " ++ file

gitPush :: FilePath -> IO ()
gitPush fp = setCurrentDirectory fp >> callCommand "git push"
