module Main where

import System.Environment (getArgs)

import Minipass.Overpass
import LambdaCalculus.TypedLambda
import LambdaCalculus.Context
import Minipass.Language.Optimiser
import Minipass.Language.Intermediate
import Minipass.Language.Program
import Utils.Parsing (parseFiles)


main :: IO ()
main = do
    args    <- getArgs
    program <- parseFiles $ args
    tri $ optimise $ typify emptyContext $ toIntermediate $ getMain $ program
