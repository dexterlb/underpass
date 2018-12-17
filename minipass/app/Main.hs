module Main where

import Minipass.Overpass
import Minipass.TypedLambda
import Minipass.Context
import Minipass.Language.Optimiser
import Minipass.Language.Intermediate
import Minipass.Language.Library

import System.IO

import qualified Data.Text as Text

import Utils.Parsing (forceParse)

main :: IO ()
main = do
    putStr "Enter query: "
    hFlush stdout
    query <- getLine
    tri $ optimise $ typify emptyContext $ toIntermediate $ forceParse parseWithLibrary $ Text.pack query
