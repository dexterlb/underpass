{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ccg.Lambda where

import           Ccg.Category
import           Ccg.Trees (ParseTree(..))

import           Utils.Maths
import           LambdaCalculus.Lambda (LambdaTerm(..))
import           LambdaCalculus.LambdaTypes (Typed, typeOf, ApplicativeType(..))

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


