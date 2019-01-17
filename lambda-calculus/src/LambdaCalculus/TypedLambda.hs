{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LambdaCalculus.TypedLambda where

import LambdaCalculus.LambdaTypes (Typed, typeOf)
import qualified LambdaCalculus.LambdaTypes as T
import LambdaCalculus.Lambda (LambdaTerm, typeOfTerm)
import qualified LambdaCalculus.Lambda as L

import Utils.Maths

import qualified Data.Text as Text

import LambdaCalculus.Context

import Data.Maybe (fromMaybe)

import Control.Exception (Exception, throw)
import Data.Dynamic (Typeable)

data TSLTerm t c where
    Constant    :: Typed c t => T.ApplicativeType t -> c           -> TSLTerm t c
    Application :: Typed c t => T.ApplicativeType t -> TSLTerm t c -> TSLTerm t c -> TSLTerm t c
    Lambda      :: Typed c t => T.ApplicativeType t -> VarName     -> TSLTerm t c -> TSLTerm t c
    Variable    :: Typed c t => T.ApplicativeType t -> Index       -> TSLTerm t c
    Cast        :: Typed c t => T.ApplicativeType t -> TSLTerm t c -> TSLTerm t c

instance Typed c t => Typed (TSLTerm t c) t where
    typeOf (Constant     t _     ) = t
    typeOf (Application  t _ _   ) = t
    typeOf (Lambda       t _ _   ) = t
    typeOf (Variable     t _     ) = t
    typeOf (Cast         t _     ) = t

instance (Show t, Show c) => Show (TSLTerm t c) where
    show = showTerm emptyContext

deriving instance (Eq c, Eq t, Typed c t) => Eq (TSLTerm t c)

showTerm :: (Show t, Show c) => VarContext t -> TSLTerm t c -> String
showTerm _ (Constant t c) = show c <> " : " <> show t
showTerm context (Application t a b) = "(" <> showTerm context a <> " . " <> showTerm context b <> ") : " <> show t
showTerm context (Lambda t x a) = "λ " <> Text.unpack x <> " { " <> showTerm (push (x, t) context) a <> " } : " <> show t
showTerm context (Variable t i)
    | Just (x, _) <- at i context = Text.unpack x <> " : " <> show t
    | otherwise = "<var " <> show i <> "> : " <> show t
showTerm context (Cast t a) = show t <> "[" <> showTerm context a <> "]"

showBareTerm :: (Show t, Show c) => VarContext t -> TSLTerm t c -> String
showBareTerm _ (Constant _ c) = show c
showBareTerm context (Application _ a b) = "(" <> showBareTerm context a <> " . " <> showBareTerm context b <> ")"
showBareTerm context (Lambda t x a) = "λ " <> Text.unpack x <> " { " <> showBareTerm (push (x, t) context) a <> " }"
showBareTerm context (Variable _ i)
    | Just (x, _) <- at i context = Text.unpack x
    | otherwise = "<var " <> show i <> ">"
showBareTerm context (Cast t a) = show t <> "[" <> showBareTerm context a <> "]"


typify :: Typed c t => VarContext t -> LambdaTerm t c -> TSLTerm t c
typify context (L.Constant c) = Constant t c
    where
        t = typeOfTerm False context (L.Constant c)
typify context (L.Application a b)
    | (p, q) <- T.inferApp ta', tb' <!> p = Application q a' b'
    | otherwise = throw $ L.CannotApply (a, ta') (b, tb')
    where
        ta' = typeOf a'
        tb' = typeOf b'
        a' = typify context a
        b' = typify context b
typify context (L.Lambda x t a) = Lambda (T.Application t (typeOf a')) x a'
    where
        a' = typify (push (x, t) context) a
typify context (L.Variable i)
    | Just (_, t) <- at i context = Variable t i
    | otherwise = throw $ L.UnknownVar i
typify context (term @ (L.Cast _ a)) = Cast (L.typeOfTerm False context term) $ typify context a

unTypify :: forall c t. (Eq t, Typed c t, MSemiLattice (T.ApplicativeType t)) => VarContext t -> TSLTerm t c -> LambdaTerm t c
unTypify context term
    | typeOf term == newType = newTerm
    | otherwise              = L.Cast (typeOf term) newTerm
    where
        newType = typeOfTerm False context newTerm :: T.ApplicativeType t
        newTerm = unTypify'        context term

        unTypify' :: (Eq t, Typed c t, MSemiLattice (T.ApplicativeType t)) => VarContext t -> TSLTerm t c -> LambdaTerm t c
        unTypify' _ (Constant _ x) = L.Constant x
        unTypify' _ (Variable _ i) = L.Variable i
        unTypify' context' (Lambda t x a)
            = L.Lambda x tx (unTypify (push (x, tx) context') a)
                where (tx, _) = T.inferApp t
        unTypify' context' (Application _ a b)
            = L.Application (unTypify context' a) (unTypify context' b)
        unTypify' context' (Cast t a) = L.Cast t (unTypify context' a)

setType :: Typed c t => T.ApplicativeType t -> TSLTerm t c -> TSLTerm t c
setType t (Constant _ x)      = Constant t x
setType t (Variable _ i)      = Variable t i
setType t (Application _ a b) = Application t a b
setType t (Lambda _ x a)      = Lambda t x a
setType t (Cast _ a)          = Cast t a

inferTypesOnClosedTerm :: (Eq c, Eq t, Typed c t, MSemiLattice (T.ApplicativeType t)) => T.ApplicativeType t -> LambdaTerm t c -> LambdaTerm t c
inferTypesOnClosedTerm t = L.removeUselessCasts . (unTypify emptyContext) . (fixedPoint fixTypes) . (setType t) . (typify emptyContext)

updateTypes :: Typed c t => (TSLTerm t c -> T.ApplicativeType t) -> TSLTerm t c -> TSLTerm t c
updateTypes updater (Constant t x) = Constant t' x
    where
        t' = updater (Constant t x)
updateTypes updater (Variable t i) = Variable t' i
    where
        t' = updater (Variable t i)
updateTypes updater (Application t a b) = Application t' a' b'
    where
        t' = updater $ Application t a' b'
        a' = updateTypes updater a
        b' = updateTypes updater b
updateTypes updater (Lambda t x a) = Lambda t' x a'
    where
        t' = updater $ Lambda t x a'
        a' = updateTypes updater a
updateTypes updater (Cast t a) = Cast t' a'
    where
        t' = updater $ Cast t a'
        a' = updateTypes updater a


fixTypes :: (Typed c t, MSemiLattice (T.ApplicativeType t)) => TSLTerm t c -> TSLTerm t c
fixTypes x = fixTypesDown (typeOf x') vars x'
    where
        (x', vars) = fixTypesUp x


fixTypesDown :: (Typed c t, MSemiLattice (T.ApplicativeType t)) => T.ApplicativeType t -> VarContext t -> TSLTerm t c -> TSLTerm t c
fixTypesDown targetType _      (Constant t x) = Constant (targetType /\ t) x
fixTypesDown targetType upVars (Variable t i)
    | Just (_, t') <- at i upVars = Variable (targetType /\ t /\ t') i
    | otherwise = throw $ L.UnknownVar i
fixTypesDown tn upVars (Lambda t x a) = Lambda (T.Application tx' ta') x a'
    where
        a'   = fixTypesDown ta' (push (x, tx') upVars) a
        tx'  = tx /\ tnx
        ta'  = ta /\ tna
        (tnx, tna) = T.inferApp tn
        (tx , ta ) = T.inferApp t
fixTypesDown tnr upVars (Application tor a b)
    | (p, _) <- T.inferApp ta     = Application tr' a' (fixTypesDown p upVars b)
    | otherwise                 = throw $ CannotApply a b
    where
        ta  = typeOf a
        a'  = fixTypesDown (T.Application tb tr') upVars a
        tr' = tnr /\ tor
        tb  = typeOf b
fixTypesDown _ upVars (Cast t a) = Cast t (fixTypesDown (typeOf a) upVars a)

fixTypesUp :: (Typed c t, MSemiLattice (T.ApplicativeType t)) => TSLTerm t c -> (TSLTerm t c, VarContext t)
fixTypesUp (Constant t x) = (Constant t x, emptyContext)
fixTypesUp (Variable t i) = (Variable t i, oneHotContext i ("", t))
fixTypesUp (Lambda t x a)
    | Just ((_, tnx), subVars) <- pop vars = (Lambda (T.Application (tnx /\ tx) (ta' /\ ta)) x a', subVars)
    | otherwise                            = (Lambda (T.Application tx (ta' /\ ta)) x a', vars)
    where
        ta' = typeOf a'
        (a', vars) = fixTypesUp a
        (tx, ta)   = T.inferApp t
fixTypesUp (Application tr a b)
    | (_, q) <- T.inferApp $ typeOf a'   = (Application (tr /\ q) a' b', vars)
    | otherwise                        = throw $ CannotApply a b
    where
        vars = meetContexts aVars bVars
        (a', aVars)  = fixTypesUp a
        (b', bVars)  = fixTypesUp b
fixTypesUp (Cast t a) = (Cast t a', aVars)
    where
        (a', aVars)  = fixTypesUp a

transformApplications :: Typed c t => ([TSLTerm t c] -> Maybe (TSLTerm t c)) -> TSLTerm t c -> TSLTerm t c
transformApplications f term = fromMaybe (g term) $ f $ uncurryApplication term
    where
        g (Application t a b) = Application t (transformApplications f a) (transformApplications f b)
        g (Lambda t x a) = Lambda t x (transformApplications f a)
        g (Cast t a) = Cast t (transformApplications f a)
        g term' = term'

transform :: Typed c t => (TSLTerm t c -> Maybe (TSLTerm t c)) -> TSLTerm t c -> TSLTerm t c
transform f term = fromMaybe (g term) $ f term
    where
        g (Application t a b) = Application t (transform f a) (transform f b)
        g (Lambda t x a) = Lambda t x (transform f a)
        g (Cast t a) = Cast t (transform f a)
        g term' = term'

uncurryApplication :: Typed c t => TSLTerm t c -> [TSLTerm t c]
uncurryApplication = reverse . uncurryApplication'
    where
        uncurryApplication' (Application _ x y) = y : uncurryApplication' x
        uncurryApplication' term = [term]

uncurryTypes :: Typed c t => TSLTerm t c -> [(TSLTerm t c, T.ApplicativeType t)]
uncurryTypes = map (\t -> (t, typeOf t)) . uncurryApplication

-- reduction and substitution:

betaReduce :: Typed c t => (TSLTerm t c -> Bool) -> TSLTerm t c -> TSLTerm t c
betaReduce p (Application t l@(Lambda (T.Application _ _) _ m) n)
    | p n = up (substitute m 0 (up n 1)) (-1)
    | otherwise = Application t (betaReduce p l) (betaReduce p n)
betaReduce _ (Application _ l@Lambda{} _) = error $ "fsck u " <> show l
betaReduce p (Lambda t x m)                   = Lambda t x    (betaReduce p m)
betaReduce p (Application t m n)              = Application t (betaReduce p m) (betaReduce p n)
betaReduce _ (Variable t x)                   = Variable t x
betaReduce _ (Constant t m)                   = Constant t m
betaReduce p (Cast t a)                       = Cast t (betaReduce p a)

substitute :: Typed c t => TSLTerm t c -> Index -> TSLTerm t c -> TSLTerm t c
substitute (Variable t x) y to
    | x == y    = to
    | otherwise = Variable t x
substitute (Application t m n) y to = Application t (substitute m y to) (substitute n y to)
substitute (Lambda t x m) y to = Lambda t x (substitute m (y + 1) (up to 1))
substitute (Constant t m) _ _  = Constant t m
substitute (Cast t m)     y to = Cast t (substitute m y to)

up :: Typed c t => TSLTerm t c -> Index -> TSLTerm t c
up to n = up' to n 0

up' :: Typed c t => TSLTerm t c -> Index -> Index -> TSLTerm t c
up' (Variable t x) d c
    | x >= c = Variable t (x + d)
    | otherwise = Variable t x
up' (Application t m n) d c = Application t (up' m d c) (up' n d c)
up' (Lambda t x m) d c = Lambda t x (up' m d (c + 1))
up' (Constant t m) _ _ = Constant t m
up' (Cast t m) d c     = Cast t (up' m d c)

data LambdaException t c
    = CannotApply (TSLTerm t c) (TSLTerm t c)
    deriving (Typeable)

deriving instance (Show t, Show c) => Show (LambdaException t c)

instance (Show t, Show c, Typeable t, Typeable c) => Exception (LambdaException t c)
