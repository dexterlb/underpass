module Main where

import System.Environment (getArgs)

import Minipass.Overpass
import LambdaCalculus.TypedLambda
import LambdaCalculus.Context
import Minipass.Language.Optimiser
import Minipass.Language.Intermediate
import Minipass.Language.Program
import Utils.Parsing (parseFile)



main :: IO ()
main = do
    [filename] <- getArgs
    program <- parseFile $ filename
    tri $ optimise $ typify emptyContext $ toIntermediate $ getMain $ program
