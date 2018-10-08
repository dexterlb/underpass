{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Context where

import LambdaTypes

import Data.Text (Text)

import Data.List (elemIndex)

type Index = Int
type VarName = Text
newtype VarContext t = VarContext [(VarName, ApplicativeType t)] deriving (Show)

push :: (VarName, ApplicativeType t) -> VarContext t -> VarContext t
push x (VarContext c) = VarContext $ x : c

pop :: VarContext t -> Maybe ((VarName, ApplicativeType t), VarContext t)
pop (VarContext (x:xs)) = Just (x, VarContext xs)
pop _                   = Nothing

oneHotContext :: MSemiLattice t => Index -> (VarName, ApplicativeType t) -> VarContext t
oneHotContext i x = VarContext $ reverse $ x : (replicate i ("", bot))

meetContexts :: MSemiLattice t => VarContext t -> VarContext t -> VarContext t
meetContexts (VarContext a) (VarContext b) = VarContext $ f a b
    where
        f ((na, ta):as) ((_, tb):bs) = (na, ta /\ tb):(f as bs)
        f [] bs = bs
        f as [] = as


at :: Index -> VarContext t -> Maybe (VarName, ApplicativeType t)
at i (VarContext c)
    | length c > i = Just $ c !! i
    | otherwise = Nothing

get :: VarName -> VarContext t -> Maybe (Index, ApplicativeType t)
get name (VarContext c) = (\i -> (i, snd $ c !! i)) <$> (elemIndex name (map fst c))

emptyContext :: VarContext t
emptyContext = VarContext []
