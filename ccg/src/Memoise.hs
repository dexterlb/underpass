module Memoise where

import           Data.Hashable     (Hashable)
import qualified Data.HashMap.Lazy as HM
import qualified Data.MemoCombinators.Class as MemoComb
import           Data.Function        (fix)
import           Control.Concurrent.MVar (newMVar, readMVar, modifyMVar_)
import           System.IO.Unsafe (unsafePerformIO)

memo :: MemoComb.MemoTable a => ((a -> b) -> a -> b) -> a -> b
memo f = g
    where
        g = MemoComb.table g'
        g' = f g

memoWith :: ((a -> b) -> (a -> b)) -> ((a -> b) -> a -> b) -> a -> b
memoWith table f = g
    where
        g = table (f g)

-- ********************
memoUgly :: (Eq a, Hashable a) => ((a -> b) -> a -> b) -> a -> b
memoUgly = memoWith tableUgly

tableUglyIO :: (Eq a, Hashable a) => (a -> b) -> IO (a -> IO b)
tableUglyIO f = do
    v <- newMVar HM.empty
    let f' x = do
            m <- readMVar v
            case HM.lookup x m of
                Nothing -> do
                    let r = f x
                    modifyMVar_ v (return . HM.insert x r)
                    return r

                Just r  -> return r
    return f'

tableUgly :: (Eq a, Hashable a) => (a -> b) -> (a -> b)
tableUgly f = let f' = unsafePerformIO (tableUglyIO f) in \ x -> unsafePerformIO (f' x)

-- ********************

fib :: (Int -> Int) -> Int -> Int
fib _ 0 = 1
fib _ 1 = 1
fib f n = (f (n - 1)) + (f (n - 2))

fib' :: Int -> Int
fib' = fix fib

fib'' :: Int -> Int
fib'' = memo fib

fib''' :: Int -> Int
fib''' = memoUgly fib
