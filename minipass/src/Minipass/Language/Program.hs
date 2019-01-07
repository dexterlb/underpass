{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Minipass.Language.Program where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import LambdaCalculus.Lambda (LambdaTerm)
import LambdaCalculus.UserTerms
import LambdaCalculus.UserTypeSystem
import Utils.Parsing (Parseable, parser, (<|>), many, try)
import Data.Bag
import Utils.Resolver

import Minipass.Language.Language
import Minipass.Language.Constants

data Statement
    = SubtypeStatement (SubtypeAssertion Types)
    | LambdaStatement  (TermDefinition Constants Types)
    deriving (Show, Eq)

data StatementKey
    = SubtypeKey
    | LambdaKey
    deriving (Show, Eq, Generic)

deriving instance Hashable StatementKey

instance Keyed Statement where
    type Key Statement = StatementKey
    keys (SubtypeStatement (SubtypeAssertion _ _)) = [SubtypeKey]
    keys (LambdaStatement  (TermDefinition   _ _)) = [LambdaKey]

instance Parseable Statement where
    parser = (try $ SubtypeStatement <$> parser) <|> (LambdaStatement <$> parser)

data Program = Program
    { types :: Library (TWR Types)
    , terms :: Library (CR Constants Types)
    }

instance Parseable Program where
    parser = (programify . fromList) <$> many parser
        where
            programify :: Bag Statement -> Program
            programify statements = Program
                { types = types'
                , terms = resolveTermLibrary types' $ termDefinitions statements
                }
                where
                    types' = resolveTypeLibrary $ subtypeAssertions statements

            subtypeAssertions :: Bag Statement -> [SubtypeAssertion Types]
            subtypeAssertions = (map extractSubtype) . (get SubtypeKey)

            termDefinitions :: Bag Statement -> [TermDefinition Constants Types]
            termDefinitions = (map extractTermDef) . (get LambdaKey)

            extractSubtype (SubtypeStatement s) = s
            extractSubtype _ = error "fu"

            extractTermDef (LambdaStatement s) = s
            extractTermDef _ = error "fu"

instance Monoid Program where
    mempty = Program { types = emptyLib TWR, terms = emptyLib CR }
    mappend
        (Program { types = types1, terms = terms1 })
        (Program { types = types2, terms = terms2 })
            = Program { types = mergeLib TWR types1 types2, terms = mergeLib CR terms1 terms2 }

getMain :: Program -> LambdaTerm Types Constants
getMain p
    | (Just term) <- getTerm "main" (terms p) = unwrap term
    | otherwise = error "no main in sight"
