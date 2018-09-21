module Main where

import Overpass
import TypedLambda
import Context
import Minipass.Optimiser
import Minipass.Intermediate
import Minipass.Language

import System.IO

main :: IO ()
main = do
    putStr "Enter query: "
    hFlush stdout
    query <- getLine
    tri $ optimise $ typify emptyContext $ toIntermediate $ pts query
