module Utils where

import System.Directory
import qualified Control.Exception as X

withDirectory :: FilePath -> IO a -> IO a
withDirectory fp oper =
  do p <- getCurrentDirectory
     setCurrentDirectory fp
     X.finally oper (setCurrentDirectory p)

quote :: String -> String
quote x = concat ["\"",x,"\""]

paren :: [String] -> [String]
paren x = ["(", concat x, ")"]
