{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module TypedLambda where

import LambdaTypes (Typed, typeOf, Unifiable, unify)
import qualified LambdaTypes as T
import Lambda (LambdaTerm, typeOfTerm)
import qualified Lambda as L

import Context

data TSLTerm t c where
    Constant    :: Typed c t => T.ApplicativeType t -> c           -> TSLTerm t c
    Application :: Typed c t => T.ApplicativeType t -> TSLTerm t c -> TSLTerm t c         -> TSLTerm t c
    Lambda      :: Typed c t => T.ApplicativeType t -> VarName     -> TSLTerm t c -> TSLTerm t c
    Variable    :: Typed c t => T.ApplicativeType t -> Index       -> TSLTerm t c

deriving instance (Typed c t, Show t, Show c) => Show (TSLTerm t c)

instance Typed c t => Typed (TSLTerm t c) t where
    typeOf (Constant     t _     ) = t
    typeOf (Application  t _ _   ) = t
    typeOf (Lambda       t _ _   ) = t
    typeOf (Variable     t _     ) = t

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
typify context (L.Lambda x t a) = Lambda (T.Application t (typeOf a')) x a'
    where
        a' = typify (push (x, t) context) a
typify context (L.Variable i)
    | Just (_, t) <- at i context = Variable t i
    | otherwise = Variable T.TypeError i

updateTypes :: Typed c t => (TSLTerm t c -> T.ApplicativeType t) -> TSLTerm t c -> TSLTerm t c
updateTypes updater (Constant t x) = Constant t' x
    where
        t' = updater (Constant t x)
updateTypes updater (Variable t i) = Variable t' i
    where
        t' = updater (Variable t i)
updateTypes updater (Application t a b) = (Application t' a b)
    where
        t' = updater $ (Application t a' b')
        a' = updateTypes updater a
        b' = updateTypes updater b
updateTypes updater (Lambda t x a) = (Lambda t' x a)
    where
        t' = updater $ Lambda t x a'
        a' = updateTypes updater a

-- here be dragons

fixTypesDown :: (Typed c t, Unifiable t) => T.ApplicativeType t -> VarContext t -> TSLTerm t c -> TSLTerm t c
fixTypesDown targetType upVars (Constant t x) = Constant (unify targetType t) x
fixTypesDown targetType upVars (Variable t i)
    | Just (_, t') <- at i upVars = Variable (unify targetType $ unify t t') i
    | otherwise = Variable T.TypeError i
fixTypesDown (T.Application tnx tna) upVars (Lambda (T.Application tx ta) x a) = Lambda (T.Application tx' ta') x a'
    where
        a'   = fixTypesDown ta' (push (x, tx') upVars) a
        tx'  = unify tx tnx
        ta'  = unify ta tna
fixTypesDown tnr upVars (Application tor a b)
    | (T.Application p _) <- typeOf a = Application tr' a' (fixTypesDown p upVars b)
    | p <- T.TypeError                = Application tr' a' (fixTypesDown p upVars b)
    where
        a'  = fixTypesDown (T.Application tb tr') upVars a
        tr' = unify tnr tor
        tb  = typeOf b

fixTypesUp :: (Typed c t, Unifiable t) => TSLTerm t c -> (TSLTerm t c, VarContext t)
fixTypesUp (Constant t x) = (Constant t x, emptyContext)
fixTypesUp (Variable t i) = (Variable t i, oneHotContext i ("", t))
fixTypesUp (Lambda (T.Application tx ta) x a)
    | Just ((_, tnx), subVars) <- pop vars = (Lambda (T.Application (unify tnx tx) (unify ta' ta)) x a', subVars)
    | otherwise                            = (Lambda T.TypeError x a', vars)
    where
        ta' = typeOf a'
        (a', vars) = fixTypesUp a
fixTypesUp (Application tr a b)
    | (T.Application p q) <- typeOf a' = (Application (unify tr q) a' b', vars)
    where
        vars = unifyContexts aVars bVars
        (a', aVars)  = fixTypesUp a
        (b', bVars)  = fixTypesUp b
