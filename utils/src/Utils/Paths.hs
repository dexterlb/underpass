module Utils.Paths where

import System.FilePath

relativeFrom :: FilePath -> FilePath -> FilePath
relativeFrom f1 f2 = joinPath [takeDirectory f1, f2]
