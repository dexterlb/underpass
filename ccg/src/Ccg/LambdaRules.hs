{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ccg.LambdaRules where

import           Ccg.Trees (ParseTree(..), tmap)
import           Ccg.Rules
import           Ccg.Modal (ModalCategory, mcmap)
import           Ccg.Lambda

import           Utils.Maths
import           LambdaCalculus.Lambda (LambdaTerm(..))
import           LambdaCalculus.LambdaTypes (Typed, Ref, UnresolvedType)
import           LambdaCalculus.UserTypeSystem (AppTypeWrapper, TypeWrapper, ConstWrapper, TypeWrappers, resolveType)
import           LambdaCalculus.UserTerms (TermLibrary, resolveTerm)


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

composeTerm :: (Eq t, PartialOrd t, Typed c t) => ParseTree (ModalCategory (AppTypeWrapper t)) (LambdaPayload (TypeWrapper t) (ConstWrapper c)) -> LambdaTerm (TypeWrapper t) (ConstWrapper c)
composeTerm = treeTerm . (tmap (\(LambdaPayload term _) -> term))
