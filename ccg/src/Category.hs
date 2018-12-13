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

module Category where

data Category atom slash
    = Atom  atom
    | Slash slash (Category atom slash) (Category atom slash)

deriving instance (Eq atom, Eq slash) => Eq (Category atom slash)

class (Finite (Rule a)) => Combines a where
    type Rule a
    combine :: a -> a -> [(Rule a, a)]
    combine x y = [(rule, z) | rule <- listAll
                             , Just z <- [combineBy rule x y]]

    combineBy :: Rule a -> a -> a -> Maybe a

class Finite a where
    listAll :: [a]

type SimpleCategory = Category String SimpleSlash

data SimpleRule
    = LeftApp
    | RightApp
    deriving (Eq)

data SimpleSlash
    = LeftSlash
    | RightSlash
    deriving (Eq)

instance Finite SimpleRule where
    listAll = [LeftApp, RightApp]

instance Combines SimpleCategory where
    type Rule SimpleCategory = SimpleRule
    combineBy LeftApp (Slash LeftSlash x y) z
        | x == z = Just y
        | otherwise = Nothing
    combineBy RightApp (Slash RightSlash x y) z
        | y == z = Just x
        | otherwise = Nothing
    combineBy _ _ _ = Nothing


