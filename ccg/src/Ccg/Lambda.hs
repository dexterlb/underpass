{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ccg.Lambda where

import           Data.Text (pack)

import           Ccg.Category
import           Ccg.Rules
import           Ccg.Trees (ParseTree(..))

import           Utils.Maths
import           LambdaCalculus.Lambda (LambdaTerm(..))
import           LambdaCalculus.LambdaTypes (Typed, typeOf, ApplicativeType(..))
import           Utils.Parsing (Parseable, parser)
import           Utils.Latex

import qualified Utils.Parsing as P

class Compositional a where
    compose :: (Typed c t) => a -> LambdaTerm t c -> LambdaTerm t c -> LambdaTerm t c

treeTerm :: forall t c cat.
            (MSemiLattice (ApplicativeType t), Typed c t, Typed cat t, Combines cat, Compositional (CombineRule cat))
         => ParseTree cat (LambdaTerm t c) -> LambdaTerm t c
treeTerm (Leaf cat term)
    | (typeOf cat :: ApplicativeType t) <!> (typeOf term :: ApplicativeType t) = error $
      "term/category inconsistency: " <> show term <> " !~ " <> show cat
    | otherwise = term
treeTerm (Vert cat rule left right)
    | (typeOf cat :: ApplicativeType t) <!> (typeOf term :: ApplicativeType t) = error $
      "term/category inconsistency at inner node: " <> show term <> " !~ " <> show cat
    | otherwise = term
    where
        term = compose rule (treeTerm left) (treeTerm right)


data LambdaPayload t c = LambdaPayload (LambdaTerm t c) MatchData

instance FromMatch (LambdaPayload t c) where
    type Constructor (LambdaPayload t c) = LambdaConstructor t c

    construct (LambdaConstructor t) = LambdaPayload t

newtype LambdaConstructor t c = LambdaConstructor (LambdaTerm t c)

instance (Typed c t, Parseable t, Parseable c) => Parseable (LambdaConstructor t c) where
    parser = do
        _    <- P.operator "@"
        term <- parser
        pure $ LambdaConstructor term

instance (Show c, Show t) => Show (LambdaPayload t c) where
    show (LambdaPayload t d) = show d <> " @ " <> show t

instance (Show c, Show t) => Show (LambdaConstructor t c) where
    show (LambdaConstructor t) = " @ " <> show t

instance (Show t, Show c) => Latexable (LambdaPayload t c) where
    latex (LambdaPayload term (MatchData { token })) = token <> " -> " <> pack (show term)
