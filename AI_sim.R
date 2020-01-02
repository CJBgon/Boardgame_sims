# In this script we simulate some board games using two functions:
# 1. deckfromtable quickly creates a deck from a short format excell/ csv sheet
# 2. gametheory is the function that plays the game. it takes several inputs
# most important of which are the decks. Naturally this function is specific 
# to the game being simulated.
# We run one sim 12 times and plot the progression of the games in plts1. We 
# also run 4 simulations with different parameters a hundred times. plts2 shows
# the recourse loss, on which action are games won and the amount of games still 
# active. 

library(data.table)
library(ggplot2)
library(gridExtra)


# functions:
deckfromtable <- function(x){
  # this function takes data.table with 4 columns: Name, Type, Power and
  # number (occurance). and creates a long format table of the deck.
  extenddeck <- c()
  retcard <- c()
  rettype <- c()
  retpow <- c()
  ret2 <- c()
  retmat <- mapply(function(c, t, p, n) {
    retcard <- c(retcard, rep(c, times = n))
    rettype <- c(rettype, rep(t, times = n))
    retpow <-  c(retpow,  rep(p, times = n))
    ret2 <- rbind(ret2, cbind(retcard,rettype,retpow))
    return(ret2)
  }, c = x[[1]], t = x[[2]], p = x[[3]], n = x[[4]],
  USE.NAMES = F,
  SIMPLIFY = F)
  deck <- rbindlist(lapply(retmat, as.data.table))
  return(deck)
}

gametheory <- function(deck = decktable,
                       aideck = ai_deck_table,
                       AI = TRUE,
                       start,
                       depletion,
                       playeractions,
                       players,
                       drawcardprob,
                       buildcards = 4) {
  # gametheory runs through the action of players and AI. It first creates 
  # decks using the deckfromtable function. Subsequently it simulates actions
  # from players, with a chance to draw a card from the deck, that can increase
  # the win condition or replenish recourses. Should the player not draw a card
  # he shall use the action to fix breaches should they run out of control. 
  # Every turn the game assumes players use all actions and that each action
  # costs resources. 
  # Following the players turns it simulates the AI drawing two cards from the
  # deck and performing the actions as designated by the cards. Right now, only 
  # Wormhole, Breach and Overdrive have been simulated. 
  #
  # Args:
  #  deck: a .csv file with information on the player deck: Name, Type,
  #   Power and number (occurance).
  #  ai_deck: same as deck but with AI cards.
  #  AI: If the AI turns should be simulated (TRUE/FALSE).
  #  start: the amount of resources each game starts with.
  #  depletion: how much resources are depleted after each action.
  #  playeractions: how much actions does each player have.
  #  players: how many players are in the game.
  #  drawcardprob: what is the probability that a player will use its action to
  #   draw a card? (0-100). e.g. 33 will simulate the player drawing a card 33% 
  #   of the time
  #  buildcards: how many of each victory cards are required to build the item?
  #
  # Returns:
  # A table with the amount of oxygen and victory items per action.  
  # declare variables.
  decklist <- deckfromtable(deck)
  ai_decklist <- deckfromtable(ai_deck_table)
  action <- 0
  breach <- 0
  builds <- list(NV <- 0,
                 EP <- 0,
                 TI <- 0,
                 KP <- 0)
  names(builds) <- c("NV", "EP", "TI", "KP")
  # each victory item requires multiple pieces to be build, to keep track of the
  # total items build we use vicbuilds:
  vicbuilds <- 0  # the sum of the items where there are 4 or more pieces for.
  pile <- data.frame()
  O2start <- start
  O2depletion <- depletion
  game <- cbind(action, O2start, vicbuilds)  # return value
  
  while (O2start > 0) {
    # each player: do actions: draw card , pass turn
    currplayer <- 0
    while (currplayer < players){
      i <- 0
      while (i < playeractions) {
        # randomly decide if the player draws a card as an action
        if(length(decklist[,retpow]) > 0){
          drawp <- sample(x= c(1:100), size = 1)
        } else {drawp = 101}
        if (drawp <= drawcardprob){
          if (length(decklist[,retpow]) >= 2){
            drew <- sample(length(decklist[,retpow]), size = 2) # draw 2 cards
            # this had to be build in case there was only on card remaining
            # in the deck, trying to draw 2 would return an error.
          } else if (length(decklist[,retpow]) == 1) {
            drew <- sample(length(decklist[,retpow]), size = 1)}
          for (card in drew){  # execute card draw action, victory builds or O2
            if (decklist[card, rettype] == "Build"){  # save build cards
              switch(decklist[card, retcard],
                     "Nano Virus" = builds$NV <- builds$NV + 1,
                     "EMP Parts" = builds$EP <- builds$EP + 1,
                     "Thermal Injectors" = builds$TI <- builds$TI + 1,
                     "Keycard Pieces" = builds$KP <- builds$KP + 1)
              # calculate new winconditions.
              vicbuilds <- sum(sapply(builds, function(x){
                ifelse(x >= buildcards, 1, 0)
              }, USE.NAMES = F))
            } else if (decklist[card, rettype] == "Time"){
              O <- sum(decklist[card, as.numeric(retpow)])
              O2start <- O2start + O  # calculate 02 additions from oxygen cards.
            }
          }
          decklist <- decklist[-c(drew),]  # remove drawn cards from the deck.
        } else if ((O2depletion + breach) > 0.5*O2start || breach > 5) {
          # if there are too many breaches, use an action on those
          breach <- breach - 1  
        }
        # we assume players use all actions, even if they do not draw a cad.
        action <- action + 1 
        i <- i + 1
      }
      currplayer <- currplayer + 1
    }
    
    if (AI == TRUE) {
      # AI turn.
      # AI draws 1 card:
      drawAI <- sample(length(ai_decklist[, retpow]), size = 1)
      aidrew <- ai_decklist[drawAI, retcard]
      # create variable earlier
      if (aidrew != "Wormhole"){pile <- rbind(pile, ai_decklist[drawAI, ])}  
      ai_decklist <- ai_decklist[-c(drawAI), ]
      
      if (aidrew == "Breach" && breach < 8){
        breach <- breach + 1
      } else if (aidrew == "Overdrive"){
        overdrive <- 1
        repeat {
          drawAI <- sample(length(ai_decklist[, retpow]), size = 1)
          aidrew <- ai_decklist[drawAI, retcard]
          if (aidrew != "Wormhole"){
            pile <- rbind(pile, ai_decklist[drawAI, ])
          }  
          ai_decklist <- ai_decklist[-c(drawAI), ]
          if (aidrew == "Breach" && breach < 8) {
            breach <- breach + 1
          } else if (aidrew == "Wormhole")  {
            ai_decklist <- rbind(ai_decklist, pile)
            pile <- data.frame()
          }
          overdrive <- overdrive + 1
          if (overdrive >= 2){
            break
          }
        }
      } else if (aidrew == "Wormhole") {
        ai_decklist <- rbind(ai_decklist, pile)
        pile <- data.frame()
      } 
    }
    # end turn, deplete oxygen
    O2start <- O2start - ((O2depletion + breach) * playeractions * players)
    # append to dataset.
    game <- rbind(game, matrix(c(action, O2start, vicbuilds), ncol = 3))
  }
  return(game)
}

# input for some calculations:
deck_table <- fread("~/Documents/path/to/item_deck.csv")
ai_deck_table <- fread("~/Documents/path/to/ai_deck.csv")
decklist <- deckfromtable(deck_table)
ai_decklist <- deckfromtable(ai_deck_table)

# run some quick probability sims on the deck ----
# the engine that will draw one in a 1000 cards.
decksamp <- replicate(1000,
                      sample(length(decklist[[1]]),
                             size  = 1,
                             replace = FALSE))
decksampai <- replicate(1000,
                        sample(length(ai_decklist[[1]]),
                               size = 1,
                               replace = F))
draws <- decklist[decksamp, ]
drawsai <- ai_decklist[decksampai, ]
# how many times is each card pulled?
drawN <- draws[, .N, by = retcard]  
drawNAI <- drawsai[, .N, by = retcard]


# Run a couple of games:
gamelist <- replicate(n = 12,
              expr = gametheory(deck = deck_table,
                                aideck = ai_deck_table,
                                AI = TRUE,
                                start = 30,
                                depletion = 1,
                                playeractions = 2,
                                players = 4,
                                drawcardprob = 33, # probability in percentage
                                buildcards = 3), 
              simplify = F)

# gamelist victories simple.
victory <- sum(sapply(gamelist, function(x) {
  ifelse(max(x[,3]) == 4, 1, 0)
}))
vicdf <- melt(data.frame("win" = victory, "loss" = length(gamelist) - victory))

# Put an identifier so that you know which table the data came from after rbind
for(i in 1:length(gamelist)){
  gamelist[[i]] <- cbind(gamelist[[i]], i)
}
df <- do.call(rbind, gamelist)

# plotting ####   
victoryplot <- ggplot(data = vicdf,
                      aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = 2, type = "qual")
  

facetplot <- ggplot(data = as.data.frame(df),
                    aes(x=action, y=O2start, color=as.factor(i))) + 
  geom_point() +
  geom_line() + 
  geom_line(data =as.data.frame(df),
            aes(x = action, y = vicbuilds, color = as.factor(i)),
            linetype = "dashed")+
  scale_color_discrete(name="Simulated Game:")+
  facet_wrap(~as.factor(i), scales = "free") +
  geom_hline(aes(yintercept = 4),
             color = "black",
             size = 0.2,
             alpha = 0.5,
             linetype = "longdash")
plts1 <- grid.arrange(facetplot, victoryplot, widths = c(6, 2))
ggsave("~/Documents/Side_Projects/Boardgames/singlesim.pdf",
       plts1,
       width = 14, height = 10, units = "in")

##### simulations:
gamelistnorm <- replicate(n = 100,
                          expr = gametheory(deck = deck_table,
                                            aideck = ai_deck_table,
                                            AI = T,
                                            start = 30,
                                            depletion = 1,
                                            playeractions = 2,
                                            players = 4,
                                            drawcardprob = 33,  
                                            buildcards = 4), 
                          simplify = F)
gamelistlong <- replicate(n = 100,
                          expr = gametheory(deck = deck_table,
                                            aideck = ai_deck_table,
                                            AI = T,
                                            start = 60,
                                            depletion = 1,
                                            playeractions = 2,
                                            players = 4,
                                            drawcardprob = 33,
                                            buildcards = 4),
                          simplify = F)
gamelistshort <- replicate(n = 100,
                           expr = gametheory(deck = deck_table,
                                             aideck = ai_deck_table,
                                             AI = T,
                                             start = 30,
                                             depletion = 2,
                                             playeractions = 2,
                                             players = 4,
                                             drawcardprob = 33,
                                             buildcards = 4),
                           simplify = F)
gamelistrapid <- replicate(n = 100,
                           expr = gametheory(deck = deck_table,
                                             aideck = ai_deck_table,
                                             AI = T,
                                             start = 30,
                                             depletion = 2,
                                             playeractions = 4,
                                             players = 4,
                                             drawcardprob = 33,
                                             buildcards = 4),
                           simplify = F)

# calculate vitories per sim.
victorylist <- sapply(list(gamelistnorm,
                           gamelistlong,
                           gamelistshort,
                           gamelistrapid), function(z) {
  victory <- sum(sapply(z, function(x) {
    ifelse(max(x[, 3]) == 4, 1, 0)
  }))
}, simplify = F)

vicdf <- as.data.table(melt(data.frame("sim" = as.factor(c(1:4)),
                         "win" = unlist(victorylist),
                         "loss" = 100 - unlist(victorylist)), id = "sim"))

# plot the victory boxplots seperately per sim
# so we can arrange them as plots in plots.
vicplot1 <- ggplot(data = vicdf[sim == 1],
                   aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = 2, type = "qual")
vicplot2 <-  ggplot(data = vicdf[sim == 2],
                    aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = 2, type = "qual")
vicplot3 <-  ggplot(data = vicdf[sim == 3],
                    aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = 2, type = "qual")
vicplot4 <-  ggplot(data = vicdf[sim == 4],
                    aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = 2, type = "qual")

sims <- list(gamelistnorm, gamelistlong, gamelistshort, gamelistrapid)
# give each game an identifier and bind them together in simlists:
simlists <- lapply(sims, function(x) {
  for(i in 1:length(x)){
  x[[i]] <- cbind(x[[i]], i)
}
  do.call(rbind, eval(x))
})

# bind together simlists ino a single dataframe, give each sim an identifier:
for(i in 1:length(simlists)){
  simlists[[i]] <- cbind(simlists[[i]], i)
}
dflists <- do.call(rbind, simlists)
colnames(dflists) <- c("action", "O2start", "vicbuilds", "game", "i")


# determine at what action games are won
# use this to plot a histogram of the data.
victoryhist <- sapply(sims, function(z) {
  victory <- sapply(z, function(x) {
    as.data.table(x)[vicbuilds == 1,][1,action]
  })
}, simplify = F)

for(i in 1:length(victoryhist)){
  victoryhist[[i]] <- cbind(victoryhist[[i]], i)
}

victoryhist <- do.call(rbind, victoryhist)
colnames(victoryhist) <- c("action", "i")

# plot several different games-paramaters:
# plotting ####
simplot <- ggplot(data = as.data.frame(dflists),
       aes(x=action, y=O2start, group = as.factor(i))) + 
  geom_point(aes(colour = as.factor(i)), alpha = 0.2) +
  stat_smooth(aes(colour = as.factor(i)), se = F, size = 1, colour = "grey34") +
  scale_color_discrete(name="Simulated Game") +
  geom_density(data = as.data.frame(dflists),stat = "count", 
               aes(x = action,
                   fill = as.factor(game),
                   group = as.factor(i),
                   colour = as.factor(i)),
               alpha = 0.5,
               inherit.aes = F) +
  facet_wrap(~i, scales = "free_x") +
  stat_count(data = as.data.frame(victoryhist), alpha = 0.3,
                 aes(x = action, colour = as.factor(i)),
             inherit.aes = F,
            )  + 
  theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1)) +
  labs(x = "Action", y = "Resource")
ggsave(filename = "~/Documents/Side_Projects/Boardgames/sims.tiff", 
       width = 19,
       height = 12,
        units = "cm")

# combine the victory boxplots and gamesim plots
v1grob <- ggplotGrob(vicplot1)
v2grob <- ggplotGrob(vicplot2)
v3grob <- ggplotGrob(vicplot3)
v4grob <- ggplotGrob(vicplot4)

xmin <- min(x); xmax <- max(x)
ymin <- min(y); ymax <- max(y)
lay <- rbind(c(1,2,2,2,3),
             c(4,2,2,2,5))
plts2 <- grid.arrange(grobs = list(v1grob, simplot, v2grob, v3grob, v4grob),
                      layout_matrix = lay)

ggsave("~/Documents/Side_Projects/Boardgames/arrangedsim.pdf",
       plts2,
       width = 14, height = 10, units = "in")


