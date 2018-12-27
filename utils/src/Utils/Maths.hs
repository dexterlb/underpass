{-# LANGUAGE FlexibleInstances #-}

module Utils.Maths where

import Data.Dynamic (Typeable)

data ProcessResult = Ok | GiveUp deriving (Show, Eq)

fixedPoint :: (Eq a) => (a -> a) -> a -> a
fixedPoint f = fst . limitedFixedPoint f 100000   -- fixme

limitedFixedPoint :: (Eq a) => (a -> a) -> Int -> a -> (a, ProcessResult)
limitedFixedPoint _ 0 x = (x, GiveUp)
limitedFixedPoint f n x
    | x == y    = (x, Ok)
    | otherwise = limitedFixedPoint f (n - 1) y
    where
        y = f x

class PartialOrd t where
    (<!)      :: t -> t -> Bool

class (Show t, Typeable t, PartialOrd t) => MSemiLattice t where
    (/\)      :: t -> t -> t           -- meet operator

class (Show t, Typeable t, PartialOrd t) => PMSemiLattice t where
    (/!\)     :: t -> t -> Maybe t     -- partial meet operator

class HasBot t where
    bot       :: t               -- x /\ bot == x

(<!>) :: PartialOrd t => t -> t -> Bool
a <!> b = a <! b || a !> b

(!>) :: PartialOrd t => t -> t -> Bool
a !> b = b <! a

instance PartialOrd String where
    (<!) = (==)

instance MSemiLattice String where
    (/\) = eqMeet

eqMeet :: (Eq a) => a -> a -> a
eqMeet a b
    | a == b    = a
    | otherwise = error "trying to meet unequal values"
