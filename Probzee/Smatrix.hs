module Probzee.Smatrix
where

import Probzee.Slist

--------------------------------------------------------------------------------
-- definitions and constructors

type Svector i a = Slist i a
type Smatrix i j a = Slist i (Slist j a)

svector :: (Eq a, Num a) => [a] -> Svector Int a
svector = slFilter (/= 0) . slistify

smatrix :: (Eq a, Num a) => [[a]] -> Smatrix Int Int a
smatrix ls = filter nonempty $ slistify $ map svector ls where
  nonempty (_,[]) = False
  nonempty _ = True
  
svectorify :: (Eq a, Num a) => [a] -> Slist Int a
svectorify = slFilter (/= 0) . slistify

svRemoveZeros :: (Eq a, Num a) => Slist i a -> Slist i a
svRemoveZeros = slFilter (/= 0)

smRemoveZeros :: (Eq a, Num a) => Smatrix i j a -> Smatrix i j a
smRemoveZeros sm = slFilter (not . null) $ slMap svRemoveZeros sm

--------------------------------------------------------------------------------

svPointwise :: (a -> b) -> Svector i a -> Svector i b
svPointwise = slMap

svPointwise2 :: (Ord i, Num a, Num b) => (a -> b -> c) ->
                                         Svector i a -> Svector i b -> Svector i c
svPointwise2 f = slUnionWith f (\a -> f a 0) (\b -> f 0 b)

smPointwise :: (a -> b) -> Smatrix i j a -> Smatrix i j b
smPointwise = slMap . svPointwise

--smPointwise2 :: (Ord i, Num a, Num b) => (a -> b -> c) ->
--                                         Smatrix i j a -> Smatrix i j b -> Smatrix i j c

smWhatever sl = matrixizedRows where
  addRowNumber i jas = map (\(j,a) -> ((j,i),a)) jas
  matrixizedRows = map (\(i,jas) -> addRowNumber i jas) sl
  matrixAll = foldl slMerge [] matrixizedRows
  unmatrixizeAll = groupBy (\((j,i),a) -> j) matrixAll
  answer = slMap (map (\((j,i),a) -> (i,a))) unmatrixizeAll


smTranspose :: (Ord i, Ord j) => Smatrix i j a -> Smatrix j i a
smTranspose sl = answer where
  addRowNumber i jas = map (\(j,a) -> ((j,i),a)) jas
  matrixizedRows = map (\(i,jas) -> addRowNumber i jas) sl
  matrixAll = foldl slMerge [] matrixizedRows
  unmatrixizeAll = groupBy (\((j,i),a) -> j) matrixAll
  answer = slMap (map (\((j,i),a) -> (i,a))) unmatrixizeAll

--------------------------------------------------------------------------------
-- arithmetic operators

svAdd :: (Ord i, Num a) => Svector i a -> Svector i a -> Svector i a
svAdd = slUnionWith (+) id id

svSubtract :: (Ord i, Num a) => Svector i a -> Svector i a -> Svector i a
svSubtract = slUnionWith (-) id negate

svNegate :: (Num a) => Svector i a -> Svector i a
svNegate = slMap negate

svDotProduct :: (Ord i, Num a) => Svector i a -> Svector i a -> a
svDotProduct xs ys = svSumEntries $ slIntersectWith (*) xs ys

smAdd :: (Ord i, Ord j, Num a) => Smatrix i j a -> Smatrix i j a -> Smatrix i j a
smAdd = slUnionWith svAdd id id

smSubtract :: (Ord i, Ord j, Num a) => Smatrix i j a -> Smatrix i j a -> Smatrix i j a
smSubtract = slUnionWith svSubtract id svNegate

smNegate :: (Ord i, Ord j, Num a) => Smatrix i j a -> Smatrix i j a
smNegate = slMap svNegate

smMultiply :: (Ord j, Ord k, Num a) => Smatrix i j a -> Smatrix j k a -> Smatrix i k a
smMultiply x y = slMap f x where
  ycols = smTranspose y
  f rowi = slMap (svDotProduct rowi) ycols
  
--------------------------------------------------------------------------------

svSumEntries :: (Num a) => Svector i a -> a
svSumEntries = slFoldl (+) 0

svNormalize :: (Fractional a) => Svector i a -> Svector i a
svNormalize sl = slMap (\a -> a / s) sl where
  s = svSumEntries sl
