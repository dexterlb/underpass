{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Underpass.Solution where

import           Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as V
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Lazy as HM
import           Control.Monad (forM_)
import           Data.Aeson (ToJSON(..), object, (.=))

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
import Utils.Maths

type Tree = ParseTree (LambdaCategory Types) (LambdaPayload (TypeWrapper Types) (ConstWrapper Constants))
type Term = LambdaTerm (TypeWrapper Types) (ConstWrapper Constants)

data Solution = Solution
    { tree     :: Tree
    , term     :: Term
    , outQuery :: Text
    } deriving (Show)

data Solutions = Solutions [TokenData] [Solution]

type UnderpassProgram = Program Types Constants

solve :: UnderpassProgram -> Text -> IO Solutions
solve p inQuery' = do
    lexer               <- simpleEnglishPosTaggingLexer
    let tokens          =  lexer inQuery'
    let categories      =  V.fromList $ match (rules p) tokens
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

summary :: Solutions -> IO ()
summary (Solutions _tokens []) = do
    putStrLn "no parses :("
summary (Solutions _tokens sols) = do
    let distinctSols = distinctBy outQuery sols
    putStrLn $ show (length sols) <> " parses, of which " <> show (length distinctSols)
            <> " distinct."

    let first = take 5 distinctSols
    putStrLn $ "here are the first " <> show (length first) <> " distinct queries:\n"
    forM_ first $ \sol -> do
        putStrLn "\n"
        Text.putStrLn $ outQuery sol

distinctBy :: (Eq k, Hashable k) => (a -> k) -> [a] -> [a]
distinctBy key = (map snd) . HM.toList . HM.fromList . (map (\x -> (key x, x)))

instance Latexable Solutions where
    latex (Solutions tokens sols)
        =  "\\section{Input query}\n"
        <> Text.intercalate " " (map latex tokens) <> "\n"
        <> "\\section{Parses}\n"
        <> (mconcat $ map (\(n, sol) -> "\\subsection{Parse " <> (pack $ show $ n + 1) <> "}\n" <> latex sol) $ indexed sols)

instance ToJSON Solutions where
    toJSON (Solutions tokens sols)
        = object [ "input" .= tokens, "parses" .= sols ]

instance Latexable Solution where
    latex (Solution { tree, term, outQuery })
        =  "\\subsubsection{Parse tree}\n" <> latex tree
        <> "\\subsubsection{Resulting term}\n"
            <> "\\begin{lstlisting}\n" <> (pack $ show term) <> "\\end{lstlisting}\n"
        <> "\\subsubsection{Output query}\n"
            <> "\\begin{lstlisting}\n" <> outQuery <> "\\end{lstlisting}\n"

instance ToJSON Solution where
    toJSON (Solution { term, outQuery })
        = object
            [ "result_term"   .= (humanJSON emptyContext $ typify emptyContext term)
            , "detyped_term"  .= (humanJSON emptyContext $ typify emptyContext $ unwrap term)
            , "reduced_term"  .= (humanJSON emptyContext $ fixedPoint (betaReduce $ const True) $ typify emptyContext $ unwrap term)
            , "output_query"  .= outQuery ]

indexed :: [a] -> [(Int, a)]
indexed = indexed' 0
    where
        indexed' _ []     = []
        indexed' n (x:xs) = ((n, x):(indexed' (n + 1) xs))
