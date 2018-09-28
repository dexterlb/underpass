{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Context where

import LambdaTypes

import Data.Text (Text)

import Data.List (elemIndex)

import Control.Monad.Fail (MonadFail)

type Index = Int
type VarName = Text
newtype VarContext t = VarContext [(VarName, ApplicativeType t)] deriving (Show)

push :: (VarName, ApplicativeType t) -> VarContext t -> VarContext t
push x (VarContext c) = VarContext $ x : c

pop :: VarContext t -> Maybe ((VarName, ApplicativeType t), VarContext t)
pop (VarContext (x:xs)) = Just (x, VarContext xs)
pop _                   = Nothing

oneHotContext :: Unifiable t => Index -> (VarName, ApplicativeType t) -> VarContext t
oneHotContext i x = VarContext $ x : (replicate i ("", top))

unifyContexts :: (Unifiable t, MonadFail m) => VarContext t -> VarContext t -> m (VarContext t)
unifyContexts (VarContext a) (VarContext b) = VarContext <$> f a b
    where
        f :: (Unifiable t, MonadFail m) => [(VarName, ApplicativeType t)] -> [(VarName, ApplicativeType t)] -> m [(VarName, ApplicativeType t)]
        f ((na, ta):as) ((_, tb):bs) = do
            unified <- unify ta tb
            rest    <- f as bs
            pure (na, unified):rest
        f [] bs = pure bs
        f as [] = pure as


at :: Index -> VarContext t -> Maybe (VarName, ApplicativeType t)
at i (VarContext c)
    | length c > i = Just $ c !! i
    | otherwise = Nothing

get :: VarName -> VarContext t -> Maybe (Index, ApplicativeType t)
get name (VarContext c) = (\i -> (i, snd $ c !! i)) <$> (elemIndex name (map fst c))

emptyContext :: VarContext t
emptyContext = VarContext []
