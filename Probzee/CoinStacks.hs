module Probzee.CoinStacks
where

import Probzee.Slist
import Probzee.Listutil

type StackSize = Integer
type Count = Integer

--------------------------------------------------------------------------------

-- A set of coin stacks is represented by a set of tuples,
-- StackSize is the number of coins in some of the stacks,
-- Count is the number of stacks that have that same size.
-- The list must be ascending in stack sizes.
-- So, if you have 20 coins in 4 piles of 5, plus one coin on the side,
-- that would be represented as [(1,1),(5,4)]
type CoinStacks = Slist StackSize Count

-- Constructors

-- given a list of stack sizes (instead of using Count,
-- you repeat the StackSize in the list), generate a CoinStacks object.
coinstacks :: [StackSize] -> CoinStacks
coinstacks ls = slMap (toInteger . length) $ groupBy id ls

csZeroCoins :: CoinStacks
csZeroCoins = []

csOneCoin :: CoinStacks
csOneCoin = [(1,1)]

csOneStack :: StackSize -> CoinStacks
csOneStack s = [(s,1)]

-- Accessors

csNumStacks :: CoinStacks -> Count
csNumStacks cs = sum $ map snd cs

csNumCoins :: CoinStacks -> Count
csNumCoins = sum . map (opOnPair (*))

--------------------------------------------------------------------------------

---- Pushes a coin onto the largest stack.
---- In case of a tie, only pushes a coin to one of the stacks that are tied for largest.
---- If stack-list passed is empty, creates a new stack.
--csPushToLargestStack :: CoinStacks -> CoinStacks
--csPushToLargestStack [] = [(1,1)]
--csPushToLargestStack ((s,1):xs) = (s+1,1) : xs
--csPushToLargestStack ((s,n):xs) = (s+1,1) : (s,n-1) : xs
  
-- Pushes one coin to create an extra stack
csPushToNewStack :: CoinStacks -> CoinStacks
csPushToNewStack ((1,n):xs) = (1,succ n) : xs
csPushToNewStack xs = (1,1) : xs
  
-- Pushes one coin onto one of the smallest stacks
csPushToSmallestStack :: CoinStacks -> CoinStacks
csPushToSmallestStack [] = error "there's no smallest stack in empty coin stacks"
csPushToSmallestStack [(s,1)] = [(succ s, 1)]
csPushToSmallestStack [(s,n)] = [(s, pred n), (succ s, 1)]
csPushToSmallestStack ((s1,1):(s2,n2):xs)
  | succ s1 == s2 = (s2, succ n2) : xs
  | otherwise     = (succ s1, 1) : (s2, n2) : xs
csPushToSmallestStack ((s1,n1):(s2,n2):xs)
  | succ s1 == s2 = (s1, pred n1) : (s2, succ n2) : xs
  | otherwise     = (s1, pred n1) : (succ s1, 1) : (s2, n2) : xs

csWaysToPushToExistingStack :: CoinStacks -> Slist CoinStacks Count
csWaysToPushToExistingStack [] = [] -- no existing stacks
csWaysToPushToExistingStack cs@[(s,n)] = [(csPushToSmallestStack cs, n)]
csWaysToPushToExistingStack cs@((s,n):xs) = (csPushToSmallestStack cs, n) : map f restWays where
  restWays = csWaysToPushToExistingStack xs
  f (otherWay,howManyWays) = ((s,n) : otherWay, howManyWays)

-- You can push one coin to any stack you want.
-- Returns a sparse list where the index is the new CoinStacks formed,
-- and the value is the number of stacks you can push the coin on
-- to get that CoinStacks object.
-- (i.e. the number of stacks of the same size where you can push
-- the coin to any one of them)
csWaysToPush :: CoinStacks -> Slist CoinStacks Count
csWaysToPush cs = (csPushToNewStack cs, 1) : csWaysToPushToExistingStack cs
  
--------------------------------------------------------------------------------

---- Remove one coin from the smallest stack
csPopFromSmallestStack :: CoinStacks -> CoinStacks
csPopFromSmallestStack [] = error "cannot pop from empty coin stacks"
csPopFromSmallestStack ((1,1):xs) = xs
csPopFromSmallestStack ((1,n):xs) = (1, pred n) : xs
csPopFromSmallestStack ((s,1):xs) = (pred s, 1) : xs
csPopFromSmallestStack ((s,n):xs) = (pred s, 1) : (s,pred n) : xs

csPopFromSecondSmallestStack :: CoinStacks -> CoinStacks
csPopFromSecondSmallestStack [] = error "cannot pop from empty coin stacks"
csPopFromSecondSmallestStack [x] = error "coin stacks not diverse enough"
csPopFromSecondSmallestStack ((s1,n1):(s2,1):xs)
  | succ s1 == s2 = (s1, succ n1) : xs
  | otherwise     = (s1, n1) : (pred s2, 1) : xs
csPopFromSecondSmallestStack ((s1,n1):(s2,n2):xs)
  | succ s1 == s2 = (s1, succ n1) : (s2, pred n2) : xs
  | otherwise     = (s1, n1) : (pred s2, 1) : (s2, pred n2) : xs

csWaysToPopFromNotSmallestStack :: CoinStacks -> Slist CoinStacks Count
csWaysToPopFromNotSmallestStack [] = []
csWaysToPopFromNotSmallestStack [x] = []
csWaysToPopFromNotSmallestStack cs@((s,n):css@((s2,n2):xs)) = (csPopFromSecondSmallestStack cs, n2) : map f restWays where
  restWays = csWaysToPopFromNotSmallestStack css
  f (otherWay,howManyWays) = ((s,n) : otherWay, howManyWays)

csWaysToPop :: CoinStacks -> Slist CoinStacks Count
csWaysToPop [] = error "cannot pop from empty coin stacks"
csWaysToPop cs@((s,n):xs) = (csPopFromSmallestStack cs, n) : csWaysToPopFromNotSmallestStack cs
  
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

--nextStackKindsWaysMatrix :: [CoinStacks] -> Slist CoinStacks (Slist CoinStacks Count)
--nextStackKindsWaysMatrix ls = slistByMap csWaysToPush ls
  
--nextStackKindsFromWaysMatrix :: Slist CoinStacks (Slist CoinStacks Count) -> [CoinStacks]
--nextStackKindsFromWaysMatrix slist = foldl (merge Descending) [] x where
--  x = map (map fst . snd) slist
  
--------------------------------------------------------------------------------

--nextStackKinds :: [CoinStacks] -> [CoinStacks]
--nextStackKinds cs = foldl (merge Descending) [] ys where
--  ys = map (slIndices . waysToPushACoin) cs
  
--allStackKinds :: [[CoinStacks]]
--allStackKinds = iterate nextStackKinds [csZeroCoins]
  
--allStackKindsWaysMatrices :: [Slist CoinStacks (Slist CoinStacks Count)]
--allStackKindsWaysMatrices = iterate f (nextStackKindsWaysMatrix [csZeroCoins]) where
--  f slist = nextStackKindsWaysMatrix $ nextStackKindsFromWaysMatrix slist

--previousStackKindsMatrix :: Slist CoinStacks (Slist CoinStacks Count) -> Slist CoinStacks [CoinStacks]
--previousStackKindsMatrix sl = answer where
--  tran = slTranspose Descending sl
--  answer = slMap slIndices tran
--  
--stackingTransitionMatrices :: [(Slist CoinStacks (Slist CoinStacks Count), Slist CoinStacks [CoinStacks])]
--stackingTransitionMatrices = map f allStackKindsWaysMatrices
--  where f m = (m, previousStackKindsMatrix m)
  
--------------------------------------------------------------------------------