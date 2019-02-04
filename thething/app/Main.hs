module Main where

import System.Environment (getArgs)
import qualified Data.Text as Text
import System.IO (hFlush, stdout)

import Underpass.Solution
import Underpass.Web (serve)

import Ccg.Program (assert)
import Utils.Parsing (parseFiles)
import Utils.Latex (latexPreview)

main :: IO ()
main = do
    sysArgs <- getArgs
    let (action:args) = case sysArgs of
                            (foo:bar) -> (foo:bar)
                            []        -> ["serve", "examples/library.ccg", "examples/test.ccg"]

    program          <- parseFiles $ args

    assert program

    case action of
        "serve" -> serve program

        _ -> do -- yeah, yeah, this is stupid, I know. Will rewrite some day.
            putStr "Enter query: "
            hFlush stdout
            query <- getLine

            result <- solve program (Text.pack query)
            case action of
                "summary" -> do
                    putStrLn "\n"
                    summary result
                "latex"   -> latexPreview result
                _         -> putStrLn $ "unknown action: " <> action
