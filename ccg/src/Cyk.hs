{-# LANGUAGE ParallelListComp #-}

module Cyk where

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.Hashable     (Hashable)
import           Data.Vector       (Vector)
import qualified Data.Vector       as Vector

import Category

type Cell cat payload = HashMap cat [Item cat payload]

data (Combines cat) => Item cat payload
    = Leaf payload
    | Vert (Rule cat) (Int, Int, cat) (Int, Int, cat)

cyk' :: (Eq cat, Hashable cat, Combines cat)
    => Vector [(cat, payload)]                  -- tagged word
    -> ((Int, Int) -> Cell cat payload)    -- recursion argument
    -> (Int, Int)                               -- word indices
    -> Cell cat payload                    -- cell at specified indices
cyk' w f (i, j)
    | i == j    = leafCell $ w Vector.! i
    | otherwise
        = aggregate
        $ foldr1 (++)
        $ map (\k -> combineCells f (i, k) (k + 1, j))
          [i .. j - 1]

combineCells :: ((Int, Int) -> Cell cat payload) -> (Int, Int) -> (Int, Int) -> [(cat, Item cat payload)]
combineCells f (a, b) (c, d)
    = [ (cat, Vert rule (a, b, x) (c, d, y))
      | (rule, cat) <- combine x y | x <- xs, y <- ys ]
    where
        xs = HM.keys $ f (a, b)
        ys = HM.keys $ f (c, d)

aggregate :: (Eq a, Hashable a) => [(a, b)] -> HashMap a [b]
aggregate = (HM.fromListWith (++)) . (map (pure <$>))

leafCell :: (Eq cat, Hashable cat) => [(cat, payload)] -> Cell cat payload
leafCell = HM.fromList . (map ((pure . Leaf) <$>))
