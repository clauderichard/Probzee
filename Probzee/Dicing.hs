{-# LANGUAGE FlexibleInstances #-}

module Probzee.Dicing
where

import Probzee.CoinStacks
import Probzee.Slist
import Probzee.Smatrix
import Probzee.Listutil
import Probzee.Probability
    
--------------------------------------------------------------------------------

type NumDice = StackSize
type NumFaces = Count
type NumRolls = Count
type Face = Count

type DiceHand = CoinStacks

type RollingLottery = Slist DiceHand Probability
type RollingLotteries = Smatrix DiceHand DiceHand Probability
type PickingChoice = [DiceHand]
type PickingChoices = Slist DiceHand PickingChoice

data Game =
  Game {
    numDice :: NumDice,
    numFaces :: NumFaces,
    numRolls :: NumRolls }

--------------------------------------------------------------------------------

intersperse :: String -> [String] -> String
intersperse i [] = ""
intersperse i [x] = x
intersperse i (x:xs) = x ++ i ++ intersperse i xs


class Reportable a where
  report :: a -> String
    
instance Reportable DiceHand where
  report [] = "empty-hand"
  --report [(2,1),(3,1)] = "full-house"
  report xs = intersperse ", " $ map f xs
    where f (s,n) = show n ++ " " ++ g s ++ (if n==1 then "" else "s")
          g 1 = "single"
          g 2 = "double"
          g 3 = "triple"
          g 4 = "quadruple"
          g 5 = "quintuple"
          g 6 = "sextuple"
          g s = show s ++ "uple"
  
instance Reportable RollingLottery where
  report ls = "\t" ++ (intersperse "\n\t" $ map f ls)
    where f (hand,prob) = report hand ++ "\n\t\tProb = " ++ show prob
    
instance Reportable RollingLotteries where
  report ls = intersperse "\n" $ map f ls
    where f (hand,lottery) = "From " ++ report hand ++ ":\n" ++ report lottery
    
--------------------------------------------------------------------------------

yahtzee = Game {
    numDice = 5,
    numFaces = 6,
    numRolls = 3 }

dhSingleton :: DiceHand
dhSingleton = csOneCoin
    
dhFullHand :: Game -> DiceHand
dhFullHand Game{numDice=nd} = [(nd,1)]
    
dhIsValid :: Game -> DiceHand -> Bool
dhIsValid (Game{numFaces=f,numDice=d}) hand
  | f < csNumStacks hand = False
  | d < csNumCoins hand = False
  | otherwise = True

--------------------------------------------------------------------------------

dhRollOneLottery :: Game -> DiceHand -> RollingLottery
dhRollOneLottery Game{numFaces=nf} dh = sort diceProbs where
  coinWaysNotNew :: Slist CoinStacks Count
  coinWaysNotNew = csWaysToPushToExistingStack dh
  numNotNew :: Count
  numNotNew = csNumStacks dh
  diceWays :: Slist DiceHand Count
  diceWays
    | nf <= numNotNew = coinWaysNotNew
    | otherwise       = (csPushToNewStack dh, nf - numNotNew) : coinWaysNotNew
  diceProbs :: Slist DiceHand Probability
  diceProbs = slMap (\x -> toRational x / toRational nf) diceWays
  
dhRollOneLotteries :: Game -> [DiceHand] -> RollingLotteries
dhRollOneLotteries game ls = slistByMap (dhRollOneLottery game) $ sort ls
  
-- goes by increasing hand size, for all possible non-full hands
dhRollOneLotteriess :: Game -> [RollingLotteries]
dhRollOneLotteriess g = take (fromInteger $ pred $ numDice g) $ iterate getNext theFirst
  where theFirst = dhRollOneLotteries g [dhSingleton]
        getNext rl = dhRollOneLotteries g $ getNextHands rl
        getNextHands rl = slIndices $ smTranspose rl
  
dhRollAllLotteriess :: Game -> RollingLotteries
dhRollAllLotteriess g = (f $ dhRollOneLotteriess g) ++ [(fullhand, [(fullhand, 1)])]
  where f :: [RollingLotteries] -> RollingLotteries
        f [x] = x
        f (x:xs) = (slMap (addSame . sort) $ smRemoveZeros $ smMultiply x theRest) ++ theRest
          where theRest = f xs
                addSame :: (Eq i, Num a) => Slist i a -> Slist i a
                addSame [] = []
                addSame [x] = [x]
                addSame ((x,m):(y,n):zs)
                  | x == y    = addSame ((x,m+n):zs)
                  | otherwise = (x,m) : addSame ((y,n):zs)
        fullhand = dhFullHand g

dhPickOne :: DiceHand -> [DiceHand]
dhPickOne dh = slIndices $ csWaysToPop dh

--------------------------------------------------------------------------------

--dhEndWinningProbs :: Game -> Slist DiceHand Probability
--dhEndWinningProbs game = 

reportRollOneLotteriess game = putStr $ concatMap (\x -> report x ++ "\n") $ dhRollOneLotteriess game
    
reportRollAllLotteriess game = putStr $ (report $ dhRollAllLotteriess game) ++ "\n"
    
--------------------------------------------------------------------------------
