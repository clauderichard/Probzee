# Probzee
Computing the probability of getting a Yahtzee

# Instructions
go to Probzee.DynamicAlgorithm, look at these 3 functions
  probzee (compute the probability of getting Yahtzee in one round of X rolls)
  probzeeD (just probzee in Double format)
  getWinningStrategyDeviations (deviations you should take from the strategy where you just keep (one of) the biggest tuple on the table after each roll).

These functions will tell you info on playing Yahtzee (but only the aiming for a Yahtzee part, not any of the other goals)
All three of them take a Game as an argument.
Use the variable "yahtzee" as the game, for standard Yahtzee.
Otherwise you can say something like: probzeeD $ Game { numDice=5, numFaces=10, numRolls=9 }
