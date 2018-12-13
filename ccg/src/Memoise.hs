module Memoise where

import           Data.Hashable     (Hashable)
import qualified Data.HashMap.Lazy as HashMap

memoise :: (Eq a, Hashable a, Enumerated a) => ((a -> b) -> a -> b) -> a -> b
memoise f x = get x
    where
        table = HashMap.fromList $ zip listAll (map (f get) listAll)
        get   = (table HashMap.!)

class Enumerated a where
    listAll :: [a]

instance Enumerated Int where
    listAll = [0..]

fib :: (Int -> Int) -> Int -> Int
fib _ 0 = 1
fib _ 1 = 1
fib f n = (f (n - 1)) + (f (n - 2))
