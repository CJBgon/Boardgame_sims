# Boardgame simulations

Using the code in AI_sim.R we simulate boardgames using two decklists and sample funcitons to determine card draws. Ultimately we want to determine the factors that impact game progression, difficulty and player impact.
We simulate some board games using two functions:

1. deckfromtable quickly creates a deck from a short format csv sheet
2. gametheory is the function that plays the game. 

it takes several inputs most important of which are the decks. Naturally this function is specific to the game being simulated. We run one simulation 12 times and plot the progression of the games in singlesim.pdf. We also run 4 simulations with different parameters a hundred times. arrangedsim.pdf shows the recourse loss, on which action are games won and the amount of games still active. 

## [single game:](figures/singlesim.pdf)
Here we plot each game simulated individualy, the dotted line is the amount of accumulated victory points, when the players get 4 of these before the resource (solid line) runs out the game counts as a win.


start resources: 30

depletion per action: 1

actions per player : 2

players: 4

probability of players drawing card: 33%

buildcards required: **3**

## [multiple games:](figures/arrangedsim.pdf)
Here we simulate a 100 games per condition. the dots are the all resource on each action from all the games, the mean resource on that action is indicated by the black line. The coloured line indicates what percentage of games are still ongoing on that action. The barplot shows the amount of games won on that action.

### sim 1
start resources: 30

depletion per action: 1

actions per player : 2

players: 4

probability of players drawing card: 33%

buildcards required: 4

### sim 2
start resources: **60**

depletion per action: 1

actions per player : 2

players: 4

probability of players drawing card: 33%

buildcards required: 4

### sim 3
start resources: 30

depletion per action: **2**

actions per player : 2

players: 4

probability of players drawing card: 33%

buildcards required: 4

### sim 4
start resources: 30

depletion per action: **2**

actions per player : **4**

players: 4

probability of players drawing card: 33%

buildcards required: 4

