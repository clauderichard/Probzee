module Probzee.Listutil
where

--------------------------------------------------------------------------------

-- assumes ascending order in each argument already
mergeBy :: (Ord e) => (a -> e) -> [a] -> [a] -> [a]
mergeBy f xs [] = xs
mergeBy f [] ys = ys
mergeBy f xss@(x:xs) yss@(y:ys)
  | f x < f y = x : mergeBy f xs yss
  | f y < f x = y : mergeBy f xss ys
  | otherwise = x : y : mergeBy f xs ys
  
-- assumes ascending order in each argument already
unionBy :: (Ord e) => (a -> e) -> [a] -> [a] -> [a]
unionBy f xs [] = xs
unionBy f [] ys = ys
unionBy f xss@(x:xs) yss@(y:ys)
  | f x < f y = x : unionBy f xs yss
  | f y < f x = y : unionBy f xss ys
  | otherwise = x : unionBy f xs ys
  
-- assumes ascending order in each argument already
merge :: (Ord a) => [a] -> [a] -> [a]
merge = mergeBy id

-- assumes ascending order in each argument already
union :: (Ord a) => [a] -> [a] -> [a]
union = unionBy id

-- assumes ascending order in each argument already
-- union a bunch of lists together
mergesBy :: (Ord e) => (a -> e) -> [[a]] -> [a]
mergesBy f lists = foldl (mergeBy f) [] lists

-- assumes ascending order in each argument already
-- union a bunch of lists together
unionsBy :: (Ord e) => (a -> e) -> [[a]] -> [a]
unionsBy f lists = foldl (unionBy f) [] lists

-- assumes ascending order in each argument already
-- union a bunch of lists together
merges :: (Ord a) => [[a]] -> [a]
merges = mergesBy id
  
-- assumes ascending order in each argument already
-- union a bunch of lists together
unions :: (Ord a) => [[a]] -> [a]
unions = unionsBy id
  
--------------------------------------------------------------------------------

-- sort by ascending order, using mergesort
sort :: (Ord a) => [a] -> [a]
sort [] = []
sort [x] = [x]
sort ls = merge (sort $ everySecondEntry ls)
                (sort $ everySecondEntry $ tail ls) where
  everySecondEntry [] = []
  everySecondEntry [x] = [x]
  everySecondEntry (x:y:zs) = x : everySecondEntry zs
  

--------------------------------------------------------------------------------

pair :: a -> b -> (a,b)
pair a b = (a,b)

opOnPair :: (a -> b -> c) -> (a,b) -> c
opOnPair f (a,b) = f a b

--------------------------------------------------------------------------------