module Git where

import Data.Text (Text)
import qualified Data.Text as Text
import System.Process (runInteractiveCommand, waitForProcess, callCommand)
import System.Directory
import System.FilePath ((</>), (<.>), addExtension, takeExtension)

import Utils

--------------------------------------------------------------------------------
--  Hacky hacky git interface, somewhat non-abstract
--  as it is tied to this use case.
--------------------------------------------------------------------------------

-- Clones a given url into a child directory of the cwd.
gitClone :: Text -> FilePath -> IO FilePath
gitClone url dir =
    do cwd <- getCurrentDirectory
       callCommand $ "git clone " ++ quote (Text.unpack url) ++ " " ++ quote dir
       return (cwd </> dir)

gitAddCommit :: FilePath -> FilePath -> IO ()
gitAddCommit repoDir file = withDirectory repoDir $ do
    putStrLn $ "\t\tgit adding and committing file: " ++ quote file
    callCommand $ "git add " ++ quote file
    callCommand $ "git commit -m 'auto-commit from the cbbgp benchmarking tool' " ++ quote file

gitPush :: FilePath -> IO ()
gitPush fp = setCurrentDirectory fp >> callCommand "git push"
