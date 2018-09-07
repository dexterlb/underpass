{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Lambda where

import qualified LambdaTypes as T
import LambdaTypes (Typed, typ)

type Index = Int
type VarName = String
newtype VarContext t = VarContext [(VarName, T.ApplicativeType t)]

push :: (VarName, T.ApplicativeType t) -> VarContext t -> VarContext t
push x (VarContext c) = VarContext $ x : c

at :: Index -> VarContext t -> Maybe (VarName, T.ApplicativeType t)
at i (VarContext c)
    | length c > i = Just $ c !! i
    | otherwise = Nothing

emptyContext :: VarContext t
emptyContext = VarContext []

data LambdaTerm t c where
    Constant    :: Typed c t => c              -> LambdaTerm t c
    Application :: Typed c t => LambdaTerm t c -> LambdaTerm t c      -> LambdaTerm t c
    Lambda      :: Typed c t => VarName        -> T.ApplicativeType t -> LambdaTerm t c -> LambdaTerm t c
    Variable    :: Typed c t => Index          -> LambdaTerm t c

instance (Show t, Show c) => Show (LambdaTerm t c) where
    show = showTerm emptyContext

showTerm :: (Show t, Show c) => VarContext t -> LambdaTerm t c -> String
showTerm _ (Constant c) = show c
showTerm context (Application a b) = "(" ++ showTerm context a ++ " " ++ showTerm context b ++ ")"
showTerm context (Lambda x t a) = "λ[" ++ x ++ ": " ++ show t ++ "] { " ++ showTerm (push (x, t) context) a ++ " }"
showTerm context (Variable i)
    | Just (x, _) <- at i context = x
    | otherwise = "<var?>"


instance (Typed c t, Eq t) => Typed (LambdaTerm t c) t where
    typ = typeOfTerm emptyContext

typeOfTerm :: (Typed c t, Eq t) => VarContext t -> LambdaTerm t c -> T.ApplicativeType t
typeOfTerm _ (Constant c) = typ c
typeOfTerm context (Application a b)
    | (T.Application p q) <- typeOfTerm context a, p == typeOfTerm context b = q
    | otherwise = T.TypeError
typeOfTerm context (Lambda x t a) = T.Application t (typeOfTerm (push (x, t) context) a)
typeOfTerm context (Variable i)
    | Just (_, t) <- at i context = t
    | otherwise = T.TypeError