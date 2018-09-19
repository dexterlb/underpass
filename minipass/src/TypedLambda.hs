{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module TypedLambda where

import LambdaTypes (Typed, typeOf, BasicUnifiable, unify)
import qualified LambdaTypes as T
import Lambda (LambdaTerm, typeOfTerm)
import qualified Lambda as L

import qualified Data.Text as Text

import Context

data TSLTerm t c where
    Constant    :: Typed c t => T.ApplicativeType t -> c           -> TSLTerm t c
    Application :: Typed c t => T.ApplicativeType t -> TSLTerm t c -> TSLTerm t c         -> TSLTerm t c
    Lambda      :: Typed c t => T.ApplicativeType t -> VarName     -> TSLTerm t c -> TSLTerm t c
    Variable    :: Typed c t => T.ApplicativeType t -> Index       -> TSLTerm t c

instance Typed c t => Typed (TSLTerm t c) t where
    typeOf (Constant     t _     ) = t
    typeOf (Application  t _ _   ) = t
    typeOf (Lambda       t _ _   ) = t
    typeOf (Variable     t _     ) = t

instance (Show t, Show c) => Show (TSLTerm t c) where
    show = showTerm emptyContext

showTerm :: (Show t, Show c) => VarContext t -> TSLTerm t c -> String
showTerm _ (Constant t c) = (show c) <> " : " <> (show t)
showTerm context (Application t a b) = "(" <> showTerm context a <> " . " <> showTerm context b <> ") : " <> (show t)
showTerm context (Lambda t x a) = "λ " <> (Text.unpack x) <> " { " <> showTerm (push (x, t) context) a <> " } : " <> (show t)
showTerm context (Variable t i)
    | Just (x, _) <- at i context = (Text.unpack x) <> " : " <> (show t)
    | otherwise = "<var " <> (show i) <> "> : " <> (show t)


typify :: Typed c t => VarContext t -> LambdaTerm t c -> TSLTerm t c
typify context (L.Constant c) = Constant t c
    where
        t = typeOfTerm context (L.Constant c)
typify context (L.Application a b)
    | (T.Application p q)  <- ta', p == tb' = (Application q a' b')
    | otherwise = Application (T.TypeError err) a' b'
    where
        err = "want to apply " <> (Text.pack $ show ta') <> " to " <> (Text.pack $ show tb')
        ta' = typeOf a'
        tb' = typeOf b'
        a' = typify context a
        b' = typify context b
typify context (L.Lambda x t a) = Lambda (T.Application t (typeOf a')) x a'
    where
        a' = typify (push (x, t) context) a
typify context (L.Variable i)
    | Just (_, t) <- at i context = Variable t i
    | otherwise = Variable (T.TypeError $ "non existant variable #" <> (Text.pack $ show i)) i

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


fixTypes :: (Typed c t, BasicUnifiable t) => TSLTerm t c -> TSLTerm t c
fixTypes x = fixTypesDown (typeOf x') vars x'
    where
        (x', vars) = fixTypesUp x

fixTypesDown :: (Typed c t, BasicUnifiable t) => T.ApplicativeType t -> VarContext t -> TSLTerm t c -> TSLTerm t c
fixTypesDown targetType upVars (Constant t x) = Constant (unify targetType t) x
fixTypesDown targetType upVars (Variable t i)
    | Just (_, t') <- at i upVars = Variable (unify targetType $ unify t t') i
    | otherwise = Variable (T.TypeError $ "non existant variable #" <> (Text.pack $ show i)) i
fixTypesDown (T.Application tnx tna) upVars (Lambda (T.Application tx ta) x a) = Lambda (T.Application tx' ta') x a'
    where
        a'   = fixTypesDown ta' (push (x, tx') upVars) a
        tx'  = unify tx tnx
        ta'  = unify ta tna
fixTypesDown tnr upVars (Application tor a b)
    | (T.Application p _) <- ta = Application tr' a' (fixTypesDown p upVars b)
    | p <- err                  = Application tr' a' (fixTypesDown p upVars b)
    where
        err = T.TypeError $ "trying to apply " <> (Text.pack $ show ta) <> " to something"
        ta  = typeOf a
        a'  = fixTypesDown (T.Application tb tr') upVars a
        tr' = unify tnr tor
        tb  = typeOf b

fixTypesUp :: (Typed c t, BasicUnifiable t) => TSLTerm t c -> (TSLTerm t c, VarContext t)
fixTypesUp (Constant t x) = (Constant t x, emptyContext)
fixTypesUp (Variable t i) = (Variable t i, oneHotContext i ("", t))
fixTypesUp (Lambda (T.Application tx ta) x a)
    | Just ((_, tnx), subVars) <- pop vars = (Lambda (T.Application (unify tnx tx) (unify ta' ta)) x a', subVars)
    | otherwise                            = (Lambda T.bottom x a', vars)
    where
        ta' = typeOf a'
        (a', vars) = fixTypesUp a
fixTypesUp (Application tr a b)
    | (T.Application p q) <- typeOf a' = (Application (unify tr q) a' b', vars)
    where
        vars = unifyContexts aVars bVars
        (a', aVars)  = fixTypesUp a
        (b', bVars)  = fixTypesUp b
