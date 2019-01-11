module Main where

import System.Environment (getArgs)
import qualified Data.Text as Text
import System.IO (hFlush, stdout)

import Underpass.Solution

import Ccg.Program (assert)
import Utils.Parsing (parseFiles)
import Utils.Latex (latexPreview)

main :: IO ()
main = do
    args    <- getArgs
    program <- parseFiles $ args

    assert program

    putStr "Enter query: "
    hFlush stdout
    query <- getLine

    latexPreview $ solve program (Text.pack query)
