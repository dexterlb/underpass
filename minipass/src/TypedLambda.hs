{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module TypedLambda where

import LambdaTypes (Typed, typeOf)
import qualified LambdaTypes as T
import Lambda (VarName, VarContext, push, at, get, Index, LambdaTerm, typeOfTerm)
import qualified Lambda as L

data TSLTerm t c where
    Constant    :: Typed c t => T.ApplicativeType t -> c           -> TSLTerm t c
    Application :: Typed c t => T.ApplicativeType t -> TSLTerm t c -> TSLTerm t c         -> TSLTerm t c
    Lambda      :: Typed c t => T.ApplicativeType t -> VarName     -> T.ApplicativeType t -> TSLTerm t c -> TSLTerm t c
    Variable    :: Typed c t => T.ApplicativeType t -> Index       -> TSLTerm t c

deriving instance (Typed c t, Show t, Show c) => Show (TSLTerm t c)

instance Typed c t => Typed (TSLTerm t c) t where
    typeOf (Constant     x _     ) = x
    typeOf (Application  x _ _   ) = x
    typeOf (Lambda       x _ _ _ ) = x
    typeOf (Variable     x _     ) = x

typify :: Typed c t => VarContext t -> LambdaTerm t c -> TSLTerm t c
typify context (L.Constant c) = Constant t c
    where
        t = typeOfTerm context (L.Constant c)
typify context (L.Application a b)
    | (T.Application p q)  <- typeOf a', p == typeOf b' = (Application q a' b')
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

-- update :: Typed c t => TSLTerm t c ->