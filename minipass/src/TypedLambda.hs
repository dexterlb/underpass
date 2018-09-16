{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module TypedLambda where

import LambdaTypes as T
import Lambda (VarName, VarContext, push, at, get, Index, LambdaTerm)
import qualified Lambda as L

data TypedLambdaTerm t c where
    Constant    :: Typed c t => t -> c                   -> TypedLambdaTerm t c
    Application :: Typed c t => t -> TypedLambdaTerm t c -> TypedLambdaTerm t c     -> TypedLambdaTerm t c
    Lambda      :: Typed c t => t -> VarName             -> T.ApplicativeType t     -> TypedLambdaTerm t c -> TypedLambdaTerm t c
    Variable    :: Typed c t => t -> Index               -> TypedLambdaTerm t c

    deriving (Show)

instance Typed c t => Typed (TypedLambdaTerm t c) t where
    typeOf Constant     t _     = t
    typeOf Application  t _ _   = t
    typeOf Lambda       t _ _ _ = t
    typeOf Variable     t _ _   = t

typify :: Typed c t => VarContext t -> LambdaTerm t c -> TypedLambdaTerm t c
typify context (L.Constant c) = typeOfTerm context (L.Constant c)
typify context (L.Application a b)
    | (T.Application p q)  <- typeOf x, p == typeOf y = (Application q a' b')
    | otherwise = Application T.TypeError a' b'
    where
        a' = typify context a
        b' = typify context b
typify context (L.Lambda x t a) = Lambda (T.Application t (typeOf a')) x t a'
    where
        a' = typify (push (x, t) context) a
typify context (L.Variable i)
    | Just (_, t) <- at i context = Variable t i
    | otherwise = Variable T.TypeError i