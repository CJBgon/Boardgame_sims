# Boardgame simulations

Using the code in AI_sim.R we simulate boardgames using two decklists and sample funcitons to determine card draws. Ultimately we want to determine the factors that impact game progression, difficulty and player impact.
We simulate some board games using two functions:

1. deckfromtable quickly creates a deck from a short format csv sheet
2. gametheory is the function that plays the game. 

it takes several inputs most important of which are the decks. Naturally this function is specific to the game being simulated. We run one simulation 12 times and plot the progression of the games in singlesim.pdf. We also run 4 simulations with different parameters a hundred times. arrangedsim.pdf shows the recourse loss, on which action are games won and the amount of games still active. 

## single game:
start resources: 30

depletion per action: 1

actions per player : 2

players: 4

probability of players drawing card: 33%

buildcards required: **3**
![alt text](figures/singlesim.pdf)

## game 1
start resources: 30

depletion per action: 1

actions per player : 2

players: 4

probability of players drawing card: 33%

buildcards required: 4

## game 2
start resources: **60**

depletion per action: 1

actions per player : 2

players: 4

probability of players drawing card: 33%

buildcards required: 4

## game 3
start resources: 30

depletion per action: **2**

actions per player : 2

players: 4

probability of players drawing card: 33%

buildcards required: 4

## game 4
start resources: 30

depletion per action: **2**

actions per player : **4**

players: 4

probability of players drawing card: 33%

buildcards required: 4




