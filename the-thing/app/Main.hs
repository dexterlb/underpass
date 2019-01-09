module Main where

import System.Environment (getArgs)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import System.IO (hFlush, stdout)

import Ccg.Program
import Ccg.Trees (ParseTree, enumTrees)
import Ccg.Lambda (LambdaPayload)
import Ccg.LambdaRules (LambdaCategory, composeTerm)
import Ccg.Cyk (cyk)
import Ccg.Rules (matchText, spaceyLexer)

import Minipass.Overpass
import LambdaCalculus.TypedLambda
import LambdaCalculus.Context
import LambdaCalculus.UserTypeSystem (unwrap, ConstWrapper, TypeWrapper)
import Minipass.Language.Optimiser
import Minipass.Language.Intermediate (toIntermediate)
import Minipass.Language.Language (Types)
import Minipass.Language.Constants (Constants)
import Utils.Parsing (parseFiles)
import Utils.Latex (latexPreview)
import LambdaCalculus.Lambda (LambdaTerm)

data Solution = Solution
    { inQuery  :: Text
    , tree     :: ParseTree (LambdaCategory Types) (LambdaPayload (TypeWrapper Types) (ConstWrapper Constants))
    , term     :: LambdaTerm (TypeWrapper Types) (ConstWrapper Constants)
    , outQuery :: Text
    } deriving (Show)

data Solutions = Solutions Text [Solution]

solve :: Program Types Constants -> Text -> Solutions
solve p inQuery = Solutions inQuery $ map makeSolution trees
    where
        makeSolution tree = Solution
            { inQuery  = inQuery
            , tree     = tree
            , term     = term
            , outQuery = tr $ optimise $ typify emptyContext $ toIntermediate $ unwrap term
            }
            where
                term = composeTerm tree

        trees = enumTrees $ cyk (V.fromList $ matchText spaceyLexer (rules p) inQuery) (begin p)

main :: IO ()
main = do
    args    <- getArgs
    program <- parseFiles $ args

    putStr "Enter query: "
    hFlush stdout
    query <- getLine

    latexPreview $ solve program (Text.pack query)
