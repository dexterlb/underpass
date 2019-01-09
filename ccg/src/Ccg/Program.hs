{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ccg.Program where

import Ccg.Lambda   (UnresolvedLambdaRule, LambdaRule, resolveLambdaRule)

import LambdaCalculus.UserTerms
import LambdaCalculus.UserTypeSystem
import LambdaCalculus.LambdaTypes (Typed)
import Utils.Parsing (Parseable, parser, (<|>), many)
import Utils.Resolver
import Utils.Maths (PartialOrd)

data Statement t c
    = SubtypeStatement (SubtypeAssertion t)
    | LambdaStatement  (TermDefinition c t)
    | MatchStatement   (UnresolvedLambdaRule t c)

deriving instance (Show t, Show c) => Show (Statement t c)

instance (Eq t, Typed c t, PartialOrd t, Parseable t, Parseable c) => Parseable (Statement t c) where
    parser = (SubtypeStatement <$> parser) <|> (LambdaStatement <$> parser)

newtype Program t c = Program [Statement t c] deriving (Monoid, Semigroup)

instance (Eq t, Typed c t, PartialOrd t, Parseable t, Parseable c) => Parseable (Program t c) where
    parser = Program <$> many parser

types :: Program t c -> Library (TWR t)
types (Program statements) = resolveTypeLibrary
    [ def | (SubtypeStatement def) <- statements ]

terms :: (Eq t, Typed c t, PartialOrd t) => Program t c -> Library (CR c t)
terms (prog @ (Program statements)) = resolveTermLibrary (types prog)
    [ def | (LambdaStatement def) <- statements]

rules :: (Eq t, PartialOrd t, Typed c t) => Program t c -> [LambdaRule t c]
rules (prog @ (Program statements)) = map (resolveLambdaRule (types prog) (terms prog))
    [ def | (MatchStatement def) <- statements]
