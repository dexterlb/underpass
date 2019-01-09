{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ccg.Lambda where

import           Ccg.Category
import           Ccg.Trees (ParseTree(..))
import           Ccg.Rules
import           Ccg.Modal (ModalCategory, mcmap)

import           Utils.Maths
import           LambdaCalculus.Lambda (LambdaTerm(..))
import           LambdaCalculus.LambdaTypes (Typed, typeOf, ApplicativeType(..), Ref, UnresolvedType)
import           LambdaCalculus.UserTypeSystem (AppTypeWrapper, TypeWrapper, ConstWrapper, TypeWrappers, resolveType)
import           LambdaCalculus.UserTerms (TermLibrary, resolveTerm)
import           Utils.Parsing (Parseable, parser)
import qualified Utils.Parsing as P

class Compositional a where
    compose :: a -> LambdaTerm t c -> LambdaTerm t c -> LambdaTerm t c

treeTerm :: forall t c cat.
            (MSemiLattice t, Typed c t, Typed cat t, Combines cat, Compositional (CombineRule cat))
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

instance (Show c, Show t) => Show (LambdaConstructor t c) where
    show (LambdaConstructor t) = " @ " <> show t

-- now more specific

type LambdaRule t c = Rule (ModalCategory (AppTypeWrapper t)) (LambdaPayload (TypeWrapper t) (ConstWrapper c))

type UnresolvedLambdaRule t c = Rule (ModalCategory (UnresolvedType t)) (LambdaPayload (Ref t) (Ref c))

resolveLambdaRule :: (Eq t, PartialOrd t, Typed c t) => TypeWrappers t -> TermLibrary c t -> UnresolvedLambdaRule t c -> LambdaRule t c
resolveLambdaRule types terms (Rule matcher items) = Rule matcher (map resolveItem items)
    where
        resolveItem (cat, LambdaConstructor term)
            = ( resolveModalCategory types cat
              , LambdaConstructor (resolveTerm types terms term) )

resolveModalCategory :: TypeWrappers t -> ModalCategory (UnresolvedType t) -> ModalCategory (AppTypeWrapper t)
resolveModalCategory types = mcmap (resolveType types)
