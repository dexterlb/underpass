{-# LANGUAGE GADTs #-}

module Ccg.Cyk where

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.Hashable     (Hashable)
import           Data.Vector       (Vector)
import qualified Data.Vector       as V
import           Data.MemoCombinators.Class (MemoTable)

import Ccg.Category
import Ccg.Trees
import Ccg.Memoise (memo)

type Cell cat payload = HashMap cat [Item cat payload]

data Item cat payload where
    Terminal :: payload -> Item cat payload
    Derive   :: (Combines cat) => CombineRule cat -> (Int, Int, cat) -> (Int, Int, cat) -> Item cat payload

cyk :: (Eq cat, Hashable cat, Combines cat, MemoTable cat)
    => Vector [(cat, payload)]                  -- tagged word
    -> cat                                      -- target category
    -> ParseForest cat payload
cyk word cat = memo (getTrees' (memo $ cyk' word)) (0, V.length word - 1, cat)

cyk' :: (Eq cat, Hashable cat, Combines cat)
    => Vector [(cat, payload)]                  -- tagged word
    -> ((Int, Int) -> Cell cat payload)         -- recursion argument
    -> (Int, Int)                               -- word indices
    -> Cell cat payload                         -- cell at specified indices
cyk' w f (i, j)
    | i == j    = leafCell $ w V.! i
    | otherwise
        = aggregate
        $ foldr1 (++)
        $ map (\k -> combineCells f (i, k) (k + 1, j))
          [i .. j - 1]

combineCells :: (Combines cat)
    => ((Int, Int) -> Cell cat payload)     -- cell getter
    -> (Int, Int)                           -- left cell index
    -> (Int, Int)                           -- right cell index
    -> [(cat, Item cat payload)]            -- produced category/item pairs
combineCells f (a, b) (c, d)
    = [ (cat, Derive rule (a, b, x) (c, d, y))
      | x <- xs, y <- ys, (rule, cat) <- combine x y]
    where
        xs = HM.keys $ f (a, b)
        ys = HM.keys $ f (c, d)

getTrees' :: (Combines cat, Eq cat, Hashable cat)
    => ((Int, Int) -> Cell cat payload)
    -> ((Int, Int, cat) -> ParseForest cat payload)  -- recursion argument
    -> (Int, Int, cat)
    -> ParseForest cat payload
getTrees' get f (i, j, cat)
    = ParseForest cat
    $ maybe [] (map buildNode) (HM.lookup cat $ get (i, j))
    where
        buildNode (Terminal payload)    = MultiLeaf payload
        buildNode (Derive   rule x y)   = MultiVert rule (f x) (f y)


aggregate :: (Eq a, Hashable a) => [(a, b)] -> HashMap a [b]
aggregate = HM.fromListWith (++) . map (pure <$>)

leafCell :: (Eq cat, Hashable cat) => [(cat, payload)] -> Cell cat payload
leafCell = HM.fromList . map ((pure . Terminal) <$>)
