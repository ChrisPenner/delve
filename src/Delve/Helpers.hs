module Delve.Helpers where

import System.Posix.Files
import System.Posix.Types
import Data.Int

getFileSize :: FilePath -> IO Int64
getFileSize fp = do
  sz <- getSymbolicLinkStatus fp
  case fileSize sz of
    COff s -> return s
