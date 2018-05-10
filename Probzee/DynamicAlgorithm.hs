module Probzee.DynamicAlgorithm
where

import Probzee.Dicing
import Probzee.Slist
import Probzee.Probability
import Probzee.Smatrix
import Probzee.Listutil

type DiceHandPayoffs = Slist DiceHand Probability

--------------------------------------------------------------------------------

daLastPayoffs :: [DiceHand] -> DiceHandPayoffs
daLastPayoffs ls = f $ sort ls
  where f [x] = [(x,1)]
        f (x:xs) = (x,0) : f xs

daTransitions :: Game -> (RollingLotteries,PickingChoices)
daTransitions game = (lots, addSelfToEach $ f $ smTranspose lots)
  where lots = dhRollAllLotteriess game
        f lotsT = slMap slIndices $ sort $ lotsT
        addSelfToEach choices = map (\(i,xs) -> (i, addAn i xs)) choices
        addAn i [] = [i]
        addAn i (x:xs)
          | i < x     = i : x : xs
          | i > x     = x : addAn i xs
          | otherwise = x : xs
        
reverseRollingPhase :: RollingLotteries -> DiceHandPayoffs -> DiceHandPayoffs
reverseRollingPhase lotteries payoffs = probsBeforeRandomEvent lotteries payoffs
        
reversePickingPhase :: PickingChoices -> DiceHandPayoffs -> Slist DiceHand (DiceHand,Probability)
reversePickingPhase choices payoffs = optimumChoices choices payoffs

reverseWholeTurn :: (RollingLotteries,PickingChoices) -> DiceHandPayoffs -> Slist DiceHand (DiceHand,Probability)
reverseWholeTurn (rls,pcs) payoffs = reversePickingPhase pcs $ reverseRollingPhase rls payoffs

reverseInfiniteTurns :: Game -> [Slist DiceHand (DiceHand,Probability)]
reverseInfiniteTurns game = iterate f firstTurn
  where rlpc@(rl,pc) = daTransitions game
        lastPayoffs = daLastPayoffs $ slIndices pc
        firstTurn = reverseWholeTurn rlpc lastPayoffs
        f turn = reverseWholeTurn rlpc $ slMap snd turn
     
-- Gives you the probability of getting a Yahtzee.
-- You can set whichever parameters you want in the game.
-- Default is yahtzee variable which is the standard one
computeWinningProbability :: Game -> Probability
computeWinningProbability game
 | numRolls game > 1 = f $ (reverseInfiniteTurns game !! (fromInteger $ pred $ pred $ numRolls game))
 | numRolls game < 1 = error "No rolls? That's not even a game, man."
 | otherwise         = thePower (toRational 1 / toRational (numFaces game)) (pred $ numDice game)
  where f choiceProbs = h $ reverseRollingPhase rl payoffs
          where payoffs = slMap snd choiceProbs
        rl = fst $ daTransitions game
        h payoffs = slAtIndex payoffs dhSingleton
        g choiceProb = snd choiceProb
        alldifferentHand = [(1,numDice game)]
        thePower x 1 = x
        thePower x e = x * thePower x (pred e)

computeWinningProbabilityD :: Game -> Double
computeWinningProbabilityD = fromRational . computeWinningProbability
        
getWinningStrategyDeviations :: Game -> [Slist DiceHand DiceHand]
getWinningStrategyDeviations game
  | numRolls game < 2 = []
  | otherwise = filterByHasDeviations $ take (pred $ fromInteger $ numRolls game) $ reverseInfiniteTurns game
  where filterByHasDeviations turns = filter (not . null) $ map deviations turns
        deviations turn = filter isStrategyDeviation $ slMap fst turn

isStrategyDeviation (hand,[]) = False
isStrategyDeviation (hand,[(x,1)]) = False
isStrategyDeviation (hand,pickedHand) = True

--------------------------------------------------------------------------------
