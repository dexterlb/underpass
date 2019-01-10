{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Underpass.Solution where

import Data.Text (Text, pack)
import qualified Data.Vector as V

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
import LambdaCalculus.Lambda (LambdaTerm)
import Utils.Latex

data Solution = Solution
    { inQuery  :: Text
    , tree     :: ParseTree (LambdaCategory Types) (LambdaPayload (TypeWrapper Types) (ConstWrapper Constants))
    , term     :: LambdaTerm (TypeWrapper Types) (ConstWrapper Constants)
    , outQuery :: Text
    } deriving (Show)

data Solutions = Solutions Text [Solution]

solve :: Program Types Constants -> Text -> Solutions
solve p inQuery' = Solutions inQuery' $ map makeSolution trees
    where
        makeSolution tree' = Solution
            { inQuery  = inQuery'
            , tree     = tree'
            , term     = term'
            , outQuery = tr $ optimise $ typify emptyContext $ toIntermediate $ unwrap term'
            }
            where
                term' = composeTerm tree'

        trees = enumTrees $ cyk (V.fromList $ matchText spaceyLexer (rules p) inQuery') (begin p)

instance Latexable Solutions where
    latex (Solutions inQuery' sols)
        =  "\\section{Input query}\n" <> inQuery' <> "\n\\section{Parses}\n"
        <> (mconcat $ map (\(n, sol) -> "\\subsection{Parse " <> (pack $ show $ n + 1) <> "}\n" <> latex sol) $ indexed sols)

instance Latexable Solution where
    latex (Solution { tree, term, outQuery })
        =  "\\subsubsection{Parse tree}\n" <> latex tree
        <> "\\subsubsection{Resulting term}\n"
            <> "\\begin{lstlisting}\n" <> (pack $ show term) <> "\\end{lstlisting}\n"
        <> "\\subsubsection{Output query}\n"
            <> "\\begin{lstlisting}\n" <> outQuery <> "\\end{lstlisting}\n"

indexed :: [a] -> [(Int, a)]
indexed = indexed' 0
    where
        indexed' _ []     = []
        indexed' n (x:xs) = ((n, x):(indexed' (n + 1) xs))