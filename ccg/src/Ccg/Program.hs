{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ccg.Program where

import qualified Data.HashMap.Lazy as HM
import Control.DeepSeq (rnf)

import Data.Text (unpack)

import Control.Monad.Fail (MonadFail)

import Ccg.LambdaRules   (UnresolvedLambdaRule, LambdaRule, LambdaCategory, UnresolvedLambdaCategory, resolveLambdaRule, resolveLambdaCategory)

import LambdaCalculus.UserTerms
import LambdaCalculus.UserTypeSystem
import LambdaCalculus.LambdaTypes (Typed)
import Utils.Parsing (Parseable, parser, (<|>))
import qualified Utils.Parsing as P
import Utils.Resolver
import Utils.Maths (PartialOrd)
import Utils.Paths (relativeFrom)


data Statement t c
    = SubtypeStatement (SubtypeAssertion t)
    | LambdaStatement  (TermDefinition c t)
    | MatchStatement   (UnresolvedLambdaRule t c)
    | BeginStatement   (UnresolvedLambdaCategory t)
    | ImportStatement   FilePath

deriving instance (Show t, Show c) => Show (Statement t c)

instance (Eq t, Typed c t, PartialOrd t, Parseable t, Parseable c) => Parseable (Statement t c) where
    parser =   beginParser <|> requireParser
           <|> (SubtypeStatement <$> parser) <|> (LambdaStatement <$> parser)
           <|> (MatchStatement <$> parser)
        where
            beginParser = P.try $ do
                _   <- P.literal "begin"
                cat <- parser
                _   <- P.operator "."
                pure $ BeginStatement cat
            requireParser = P.try $ do
                _       <- P.literal "import"
                path    <- P.quotedString '"'
                _       <- P.operator "."
                pure $ ImportStatement $ unpack path

newtype Program t c = Program [Statement t c] deriving (Monoid, Semigroup)

instance (Eq t, Typed c t, PartialOrd t, Parseable t, Parseable c) => Parseable (Program t c) where
    parser = Program <$> P.many parser

types :: Program t c -> Library (TWR t)
types (Program statements) = resolveTypeLibrary
    [ def | (SubtypeStatement def) <- statements ]

terms :: (Eq t, Typed c t, PartialOrd t) => Program t c -> Library (CR c t)
terms (prog @ (Program statements)) = resolveTermLibrary (types prog)
    [ def | (LambdaStatement def) <- statements]

rules :: (Eq c, Eq t, PartialOrd t, Typed c t) => Program t c -> [LambdaRule t c]
rules (prog @ (Program statements)) = map (resolveLambdaRule (types prog) (terms prog))
    [ def | (MatchStatement def) <- statements]

imports :: (Eq c, Eq t, PartialOrd t, Typed c t) => Program t c -> [FilePath]
imports (Program statements) =
    [ def | (ImportStatement def) <- statements]

begin :: Program t c -> LambdaCategory t
begin (prog @ (Program statements))
    | [beg] <- begins = resolveLambdaCategory (types prog) beg
    | []    <- begins = error "no begin in sight"
    | otherwise       = error "too many begins"
    where
        begins = [ beg | (BeginStatement beg) <- statements ]

assert :: (Eq c, Eq t, Typed c t, PartialOrd t, MonadFail m) => Program t c -> m ()
assert p = do
    pure $! rnf (show p)

instance (Show t, Show c, Eq c, Eq t, Typed c t, PartialOrd t) => Show (Program t c) where
    show prog = (showL "types" $ HM.toList $ types prog)
             <> (showL "terms" $ HM.toList $ terms prog)
             <> (showL "rules" $ rules prog)
             <> (showL "begin" $ [begin prog])

showL :: (Show a) => String -> [a] -> String
showL s = ((s <> ":\n") <>) . (<> "\n") . unlines . (map show)

load :: (Eq c, Eq t, PartialOrd t, Typed c t, Parseable t, Parseable c) => FilePath -> IO (Program t c)
load f = do
    prog <- P.parseFile f
    deps <- mapM load $ map (relativeFrom f) $ imports prog
    pure $ mconcat (prog:deps)
