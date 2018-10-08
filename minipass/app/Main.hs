module Main where

import Overpass
import TypedLambda
import Context
import Minipass.Optimiser
import Minipass.Intermediate
import Minipass.Library

import System.IO

import qualified Data.Text as Text

import Parsing (forceParse)

main :: IO ()
main = do
    putStr "Enter query: "
    hFlush stdout
    query <- getLine
    tri $ optimise $ typify emptyContext $ toIntermediate $ forceParse parseWithLibrary $ Text.pack query
