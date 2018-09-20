module Maths where

data ProcessResult = Ok | GiveUp deriving (Show, Eq)

fixedPoint :: (Eq a) => (a -> a) -> a -> a
fixedPoint f = fst . (limitedFixedPoint f 100000)   -- fixme

limitedFixedPoint :: (Eq a) => (a -> a) -> Int -> a -> (a, ProcessResult)
limitedFixedPoint _ 0 x = (x, GiveUp)
limitedFixedPoint f n x
    | x == y    = (x, Ok)
    | otherwise = limitedFixedPoint f (n - 1) y
    where
        y = f x
