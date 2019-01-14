{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Ccg.LambdaRules where

import           Control.Exception (Exception, throw)
import           Data.Dynamic (Typeable)

import           Ccg.Trees (ParseTree(..), tmap)
import           Ccg.Rules
import           Ccg.Modal (ModalCategory, mcmap)
import           Ccg.Lambda

import           Utils.Maths
import           LambdaCalculus.Lambda (LambdaTerm(..))
import           LambdaCalculus.LambdaTypes (Typed, Ref, UnresolvedType, typeOf)
import           LambdaCalculus.UserTypeSystem (AppTypeWrapper, TypeWrapper, ConstWrapper, TypeWrappers, resolveType)
import           LambdaCalculus.UserTerms (TermLibrary, resolveTerm)


type LambdaRule t c = Rule (ModalCategory (AppTypeWrapper t)) (LambdaPayload (TypeWrapper t) (ConstWrapper c))
type UnresolvedLambdaRule t c = Rule (ModalCategory (UnresolvedType t)) (LambdaPayload (Ref t) (Ref c))

type LambdaCategory t = ModalCategory (AppTypeWrapper t)
type UnresolvedLambdaCategory t = ModalCategory (UnresolvedType t)

resolveLambdaRule :: forall c t. (Eq c, Eq t, PartialOrd t, Typed c t) => TypeWrappers t -> TermLibrary c t -> UnresolvedLambdaRule t c -> LambdaRule t c
resolveLambdaRule types terms (Rule matcher items) = Rule matcher (map (resolveItem) items)
    where
        resolveItem (cat, LambdaConstructor term)
            = check $ ( resolvedCat
-- need a simpler, hackier type inference which doesn't mess up cross-branch casts
--              , LambdaConstructor (inferTypesOnClosedTerm (typeOf resolvedCat) $ resolveTerm types terms term) )
              , LambdaConstructor (resolveTerm types terms term) )
                where
                    resolvedCat = resolveLambdaCategory types cat


        check :: (ModalCategory (AppTypeWrapper t), LambdaConstructor (TypeWrapper t) (ConstWrapper c)) ->
                 (ModalCategory (AppTypeWrapper t), LambdaConstructor (TypeWrapper t) (ConstWrapper c))

        check (x @ (cat, LambdaConstructor term))
            | typeOfCat == typeOf term  = x
            | otherwise                 = throw $ TypeMismatch (cat, typeOfCat) (term, typeOf term)
            where
                -- haskell is dumb and can't find this type by himself
                typeOfCat = typeOf cat :: AppTypeWrapper t

resolveLambdaCategory :: TypeWrappers t -> ModalCategory (UnresolvedType t) -> ModalCategory (AppTypeWrapper t)
resolveLambdaCategory types = mcmap (resolveType types)

composeTerm :: (Eq t, PartialOrd t, Typed c t) => ParseTree (ModalCategory (AppTypeWrapper t)) (LambdaPayload (TypeWrapper t) (ConstWrapper c)) -> LambdaTerm (TypeWrapper t) (ConstWrapper c)
composeTerm = treeTerm . (tmap (\(LambdaPayload term _) -> term))

data LambdaRuleException t c
    = TypeMismatch (LambdaCategory t, AppTypeWrapper t) (LambdaTerm (TypeWrapper t) (ConstWrapper c), AppTypeWrapper t)
    deriving (Typeable)

deriving instance (Show t, Show c) => Show (LambdaRuleException t c)

instance (Show t, Show c, Typeable t, Typeable c) => Exception (LambdaRuleException t c)
