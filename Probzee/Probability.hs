module Probzee.Probability
where

import Probzee.Slist
import Probzee.Smatrix

type Probability = Rational
type RandomEvent a = Slist a Probability

--------------------------------------------------------------------------------

-- Given a lottery on a (arg 1), and for each a the probability of winning from there,
-- returns the probability of winning before that lottery is cast.
probBeforeRandomEvent :: (Ord a) => RandomEvent a -> Slist a Probability -> Probability
probBeforeRandomEvent probs exps = svSumEntries $ slIntersectWith (*) probs exps

-- For each b you can choose, there's a transition to a lottery on a. That's arg 1.
-- Arg 2 is, for each a you can end up on, your probability of winning from that a.
-- Returns, for each b, the probability of winning from that b.
probsBeforeRandomEvent :: (Ord a) => Slist b (RandomEvent a) -> Slist a Probability -> Slist b Probability
probsBeforeRandomEvent startingPoints exps = slMap (flip probBeforeRandomEvent exps) startingPoints

-- For each b you can choose, there's a transition to a lottery on a. That's arg 1.
-- Arg 2 is, for each a you can end up on, your probability of winning from that a.
-- Returns which b you should choose to maximize your probability of winning.
-- Also returns the probability of winning from b, in the tuple.
chooseMaximumProb :: (Ord a) => Slist b (RandomEvent a) -> Slist a Probability -> (b,Probability)
chooseMaximumProb startingPoints exps = slMaximum $ probsBeforeRandomEvent startingPoints exps

-- Arg1: for each a, can choose which b to go to
-- Arg2: for each b, get an expected payoff
-- return, for each a, which b to choose and the resulting expected payoff
optimumChoices :: (Ord a, Ord b) => Slist a [b] -> Slist b Probability -> Slist a (b,Probability)
optimumChoices choices exps = slMap f choices
  where f bs = slMaximum $ choiceProbs bs
        choiceProbs bs = slRestrict bs exps

---- Given a lottery on a, and for each a a lottery on b,
---- returns the lottery on b that you get by picking randomly from the first lottery
---- then from there picking randomly from the corresponding second lottery.
--probChain :: RandomEvent a -> Slist a (RandomEvent b) -> RandomEvent b
--

--------------------------------------------------------------------------------
