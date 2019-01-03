{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Bag
    ( Bag
    , Keyed(..)
    , add, get, empty, fromList
    ) where

-- import qualified Data.HashMap as HM
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.Hashable (Hashable)
import           Data.List (foldl')

class (Hashable (Key a), Eq (Key a)) => Keyed a where
    type Key a
    keys :: a -> [Key a]


newtype Bag a = Bag (HashMap (Key a) [a])

fromList :: Keyed a => [a] -> Bag a
fromList = foldl' (flip add) empty

add :: Keyed a => a -> Bag a -> Bag a
add x b = foldl' (addAt x) b (keys x)
    where
        addAt :: Keyed a => a -> Bag a -> Key a -> Bag a
        addAt x' (Bag m) k = Bag $ HM.insertWith (++) k [x'] m

get :: Keyed a => Key a -> Bag a -> [a]
get k (Bag m) = HM.lookupDefault [] k m

empty :: Bag a
empty = Bag $ HM.empty
