{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Minipass.Language.Program where

import Data.Maybe (catMaybes)

import LambdaCalculus.Lambda (LambdaTerm)
import LambdaCalculus.UserTerms
import LambdaCalculus.UserTypeSystem
import Utils.Parsing (Parseable, parser, (<|>), many, try)
import Utils.Resolver

import Minipass.Language.Language
import Minipass.Language.Constants

data Statement
    = SubtypeStatement (SubtypeAssertion Types)
    | LambdaStatement  (TermDefinition Constants Types)
    deriving (Show, Eq)

instance Parseable Statement where
    parser = (try $ SubtypeStatement <$> parser) <|> (LambdaStatement <$> parser)

data Program = Program
    { types :: Library (TWR Types)
    , terms :: Library (CR Constants Types)
    }

instance Parseable Program where
    parser = programify <$> many parser
        where
            programify :: [Statement] -> Program
            programify statements = Program
                { types = types'
                , terms = resolveTermLibrary types' $ termDefinitions statements
                }
                where
                    types' = resolveTypeLibrary $ subtypeAssertions statements

            subtypeAssertions :: [Statement] -> [SubtypeAssertion Types]
            subtypeAssertions = catMaybes . (map extractSubtype)

            termDefinitions :: [Statement] -> [TermDefinition Constants Types]
            termDefinitions = catMaybes . (map extractTermDef)

            extractSubtype (SubtypeStatement s) = Just s
            extractSubtype _ = Nothing

            extractTermDef (LambdaStatement s) = Just s
            extractTermDef _ = Nothing

instance Monoid Program where
    mempty = Program { types = emptyLib TWR, terms = emptyLib CR }

instance Semigroup Program where
    (Program { types = types1, terms = terms1 }) <> (Program { types = types2, terms = terms2 })
        = Program { types = mergeLib TWR types1 types2, terms = mergeLib CR terms1 terms2 }

getMain :: Program -> LambdaTerm Types Constants
getMain p
    | (Just term) <- getTerm "main" (terms p) = unwrap term
    | otherwise = error "no main in sight"
