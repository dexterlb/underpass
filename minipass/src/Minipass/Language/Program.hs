{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Minipass.Language.Program where

import LambdaCalculus.Lambda (LambdaTerm)
import LambdaCalculus.UserTerms
import LambdaCalculus.UserTypeSystem
import Utils.Parsing (Parseable, parser, (<|>), many)
import Utils.Resolver

import Minipass.Language.Language
import Minipass.Language.Constants

data Statement
    = SubtypeStatement (SubtypeAssertion Types)
    | LambdaStatement  (TermDefinition Constants Types)
    deriving (Show, Eq)

instance Parseable Statement where
    parser = (SubtypeStatement <$> parser) <|> (LambdaStatement <$> parser)

newtype Program = Program [Statement] deriving (Monoid, Semigroup)

instance Parseable Program where
    parser = Program <$> many parser

types :: Program -> Library (TWR Types)
types (Program statements) = resolveTypeLibrary
    [ def | (SubtypeStatement def) <- statements ]

terms :: Program -> Library (CR Constants Types)
terms (prog @ (Program statements)) = resolveTermLibrary (types prog)
    [ def | (LambdaStatement def) <- statements]

getMain :: Program -> LambdaTerm Types Constants
getMain p
    | (Just term) <- getTerm "main" (terms p) = unwrap term
    | otherwise = error "no main in sight"
