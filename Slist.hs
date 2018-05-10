module Probzee.Slist
where

import Probzee.Listutil

--------------------------------------------------------------------------------
-- Slist definition and constructors

type Slist i a = [(i,a)]

slistByMap :: (a -> b) -> [a] -> Slist a b
slistByMap f = map (\a -> (a,f a))

slistify :: [a] -> Slist Int a
slistify = zipWith pair [0..]

-- Assumes the list is already sorted by e, or at least all e's that are
-- equal are adjacent to each other in the list
groupBy :: (Eq e) => (a -> e) -> [a] -> Slist e [a]
groupBy f ls = slMap reverse $ gun ls where
  gun [] = []
  gun (x:xs) = fun (f x) [x] xs
  fun cur acc [] = [(cur,acc)]
  fun cur acc (x:xs)
    | cur == f x  = fun cur (x:acc) xs
    | otherwise   = (cur,acc) : fun (f x) [x] xs
  
--------------------------------------------------------------------------------
-- accessors

slIndices :: Slist i a -> [i]
slIndices = map fst

slEntries :: Slist i a -> [a]
slEntries = map snd

-- expand slist into a list of a
-- according to the indices arg3
-- empty values are arg1 (maybe zeros)
slExpand :: (Ord i) => a -> [i] -> Slist i a -> [a]
slExpand _ [] _ = []
slExpand z indices [] = map (\j -> z) indices
slExpand z indices@(j:js) xss@((i,a):xs)
  | i < j     = slExpand z indices xs
  | i > j     = z : slExpand z js xss
  | otherwise = a : slExpand z js xs
  
-- contract slist by restricting to only those indices
-- that are specified
slRestrict :: (Ord i) => [i] -> Slist i a -> Slist i a
slRestrict [] _ = []
slRestrict _ [] = []
slRestrict jss@(j:js) xss@((i,x):xs)
  | j < i     = slRestrict js xss
  | j > i     = slRestrict jss xs
  | otherwise = (i,x) : slRestrict js xs

slAtIndex :: (Eq i) => Slist i a -> i -> a
slAtIndex ls i = snd $ head $ filter (\(j,x) -> i == j) ls
  
--------------------------------------------------------------------------------
-- basic list functions, sparse version

slFilter :: (a -> Bool) -> Slist i a -> Slist i a
slFilter f [] = []
slFilter f ((i,a):xs)
  | f a       = (i,a) : slFilter f xs
  | otherwise = slFilter f xs

slMap :: (a -> b) -> Slist i a -> Slist i b
slMap f = map (\(i,a) -> (i,f a))

slFoldl :: (z -> a -> z) -> z -> Slist i a -> z
slFoldl f z [] = z
slFoldl f z ((i,a):xs) = slFoldl f (f z a) xs

slFoldr :: (a -> z -> z) -> Slist i a -> z -> z
slFoldr f [] z = z
slFoldr f ((i,a):xs) z = f a $ slFoldr f xs z

--TODO slFoldlAll
slFoldlAll :: (Ord i) => (z -> a -> z) -> z -> (Slist i a, a, [i]) -> z
slFoldlAll f z (sl,nullentry,indices) = foldl f z $ slExpand nullentry indices sl

--TODO slFoldrAll
slFoldrAll :: (Ord i) => (a -> z -> z) -> z -> (Slist i a, a, [i]) -> z
slFoldrAll f z (sl,nullentry,indices) = foldr f z (slExpand nullentry indices sl)

--------------------------------------------------------------------------------
--zip clones

slIntersectWith :: (Ord i) => (a -> b -> c) -> Slist i a -> Slist i b -> Slist i c
slIntersectWith _ [] _ = []
slIntersectWith _ _ [] = []
slIntersectWith f xss@((i,x):xs) yss@((j,y):ys)
  | i < j     = slIntersectWith f xs yss
  | j < i     = slIntersectWith f xss ys
  | otherwise = (i,f x y) : slIntersectWith f xs ys

slIntersect :: (Ord i) => Slist i a -> Slist i b -> Slist i (a,b)
slIntersect = slIntersectWith pair
  
slUnionWith :: (Ord i) => (a -> b -> c) -> (a -> c) -> (b -> c) ->
                          Slist i a -> Slist i b -> Slist i c
slUnionWith _ _ _ [] _ = []
slUnionWith _ _ _ _ [] = []
slUnionWith f ac bc xss@((i,x):xs) yss@((j,y):ys)
  | i < j     = (i,ac x) : slUnionWith f ac bc xs yss
  | j < i     = (j,bc y) : slUnionWith f ac bc xss ys
  | otherwise = (i,f x y) : slUnionWith f ac bc xs ys

slUnion :: (Ord i) => Slist i a -> Slist i b -> Slist i (Maybe a, Maybe b)
slUnion = slUnionWith (\a b -> (Just a, Just b))
                      (\a -> (Just a, Nothing))
                      (\b -> (Nothing, Just b))
  
slMergeWith :: (Ord i) => (a -> c) -> (b -> c) ->
                          Slist i a -> Slist i b -> Slist i c
slMergeWith ac bc [] ys = slMap bc ys
slMergeWith ac bc xs [] = slMap ac xs
slMergeWith ac bc xss@((i,x):xs) yss@((j,y):ys)
  | i < j     = (i,ac x) : slMergeWith ac bc xs yss
  | j < i     = (j,bc y) : slMergeWith ac bc xss ys
  | otherwise = (i,ac x) : (j,bc y) : slMergeWith ac bc xs ys

slMerge :: (Ord i) => Slist i a -> Slist i a -> Slist i a
slMerge = slMergeWith id id
  
--------------------------------------------------------------------------------

slSetEntry :: (Ord i) => i -> a -> Slist i a -> Slist i a
slSetEntry i a [] = []
slSetEntry i a ((j,b):ls)
  | i < j     = (i,a) : (j,b) : ls -- already passed
  | j < i     = (j,b) : slSetEntry i a ls -- not there yet
  | otherwise = (j,a) : ls -- we're here
  
slMapEntry :: (Ord i) => i -> (Maybe a -> a) -> Slist i a -> Slist i a
slMapEntry i f [] = []
slMapEntry i f ((j,b):ls)
  | i < j     = (i, f Nothing) : (j,b) : ls -- already passed
  | j < i     = (j,b) : slMapEntry i f ls -- not there yet
  | otherwise = (j, f (Just b)) : ls -- we're here

slMaximum :: (Ord a) => Slist i a -> (i,a)
slMaximum [] = error "cannot take max index of empty list"
slMaximum (z:zs) = f z zs where
  f z [] = z
  f (i,x) ((j,y):ys)
    | x < y     = f (j,y) ys
    | otherwise = f (i,x) ys
  