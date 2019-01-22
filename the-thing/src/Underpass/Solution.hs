{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Underpass.Solution where

import           Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Vector as V

import Ccg.Program
import Ccg.Trees (ParseTree, enumTrees)
import Ccg.Lambda (LambdaPayload)
import Ccg.LambdaRules (LambdaCategory, composeTerm)
import Ccg.Cyk (cyk)
import Ccg.Rules (match, TokenData)
import Ccg.POSTagging (simpleEnglishPosTaggingLexer)

import Minipass.Overpass
import LambdaCalculus.TypedLambda
import LambdaCalculus.LambdaTypes (ApplicativeType(..))
import LambdaCalculus.Context
import LambdaCalculus.UserTypeSystem (unwrap, ConstWrapper, TypeWrapper)
import Minipass.Language.Optimiser
import Minipass.Language.Intermediate (toIntermediate)
import Minipass.Language.Language (Types)
import Minipass.Language.Constants (Constants)
import LambdaCalculus.Lambda (LambdaTerm)
import Utils.Latex

type Tree = ParseTree (LambdaCategory Types) (LambdaPayload (TypeWrapper Types) (ConstWrapper Constants))
type Term = LambdaTerm (TypeWrapper Types) (ConstWrapper Constants)

data Solution = Solution
    { tree     :: Tree
    , term     :: Term
    , outQuery :: Text
    } deriving (Show)

data Solutions = Solutions [TokenData] [Solution]

solve :: Program Types Constants -> Text -> IO Solutions
solve p inQuery' = do
    lexer               <- simpleEnglishPosTaggingLexer
    let tokens          =  lexer inQuery'
    let categories      =  V.fromList $ match (rules p) tokens
    putStrLn $ show categories
    let trees           =  enumTrees $ cyk categories (begin p)
    pure $ Solutions tokens $ map makeSolution trees

makeSolution :: Tree -> Solution
makeSolution tree' =
    Solution
        { tree     = tree'
        , term     = term'
        , outQuery = tr $ optimise $ typify emptyContext $ toIntermediate $ unwrap term'
        }
        where
            term' = inferTypesOnClosedTerm Wildcard $ composeTerm tree'

instance Latexable Solutions where
    latex (Solutions tokens sols)
        =  "\\section{Input query}\n"
        <> Text.intercalate " " (map latex tokens) <> "\n"
        <> "\\section{Parses}\n"
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
