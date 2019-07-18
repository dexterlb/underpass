module Main where

import System.Environment (getArgs)
import qualified Data.Text as Text
import System.IO (hFlush, stdout)

import Underpass.Solution
import Underpass.Web (serve)

import Ccg.Program (assert, load)
import Utils.Latex (latexPreview)

main :: IO ()
main = do
    sysArgs <- getArgs
    let (action:arg:rest) = case sysArgs of
                            (foo:bar) -> (foo:bar)
                            []        -> ["serve", "examples/sample_grammar.ccg"]

    program          <- load arg

    assert program

    case action of
        "serve" -> serve program

        _ -> do -- yeah, yeah, this is stupid, I know. Will rewrite some day.
            query <- case rest of
              [] -> do
                putStr "Enter query: "
                hFlush stdout
                query <- getLine
                return query
              q:[] -> pure q
              _ -> error "unknown number of arguments"

            result <- solve program (Text.pack query)
            case action of
                "summary" -> do
                    putStrLn "\n"
                    summary result
                "latex"   -> latexPreview result
                _         -> putStrLn $ "unknown action: " <> action
