{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graph where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad
import qualified Control.Exception as X
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import Graphics.Gnuplot.Simple (plotLists, Attribute(..))
import System.FilePath ((</>), (<.>), addExtension, takeExtension)
import System.Directory
import Data.List (sort,intersperse)

import Utils

type Tag = String
type DiffTime = Double

-- Read in any old results and graph all results together, replacing any
-- current graph at "$repo/$project/$name.{time,memory}.svg".
--
-- Pre-existing files should be $repo/$project/*.raw.results and be readable via
-- @read :: String -> (TagText,HashMap Text (Double, DiffTime))@ (i.e. produced via 'show').
--
-- XXX The over-use of tuples makes this long-in-the-tooth. I should make a small ADT.
doGraph :: FilePath -> String -> IO [FilePath]
doGraph repoDir projectName =
  do numbers <- readOldResults
     let graphs  = extractGraphData numbers
     concat <$> mapM writeGraph graphs
 where
    extractGraphData :: [(Tag,HashMap Text (Double, DiffTime))] -> [(Text, [(Tag,Double,DiffTime)])]
    extractGraphData ds =
        let (tags,maps) = unzip ds
            allKeys     = HashMap.keys (HashMap.unions maps)
        in [(k, [ (tag,mem,time) | Just (mem,time) <- map (HashMap.lookup k) maps | tag <- tags]) | k <- allKeys]

    readOldResults :: IO [(Tag, HashMap Text (Double, DiffTime))]
    readOldResults =
      do let dir = repoDir </> projectName
             isResult = (".results" ==) . takeExtension
         resultFiles <- (sort . filter isResult) <$> getDirectoryContents dir
         mapM (fmap read . readFile . (dir </>)) resultFiles

    writeGraph :: (Text, [(Tag,Double,DiffTime)]) -> IO [FilePath]
    writeGraph (benchName, tmts) = do
        let (tags, mems,times) = unzip3 tmts
            renderedTags = paren $ intersperse "," $
                                [quote t ++ " " ++ show n | (Just t,n) <- zip (impulse tags) [0..]]
            plot fp xs lbl = plotLists [ XTicks (Just renderedTags)
                                       , XLabel "Version"
                                       , YLabel lbl
                                       , YRange (0, 1.1 * maximum xs)
                                       , Custom "nokey" []
                                       , Custom "terminal" ["svg"]
                                       , Custom "output" [quote fp]] [xs]
            prefix x = repoDir </> projectName </> Text.unpack benchName <.> x
            fpMS     = prefix $ "memory" <.> "svg"
            fpTS     = prefix $ "time"   <.> "svg"
        plot fpMS mems "Bytes"
        plot fpTS times "Seconds"
        return [fpMS,fpTS]

    -- Retain those the tags that differ from the immediately preceding tag.
    impulse :: [Tag] -> [Maybe Tag]
    impulse xs@(x:ys) = Just x : zipWith (\p c -> if p == c then Nothing else Just c) xs ys
