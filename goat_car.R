# Simulation of the America TV show "Monty Hall" where the host invites guests to select one of three doors  behind which there are 2 goats and 1 car hidden.
# The game show is explained here: https://en.wikipedia.org/wiki/Monty_Hall_problem
# The following simulation shows out of 1000 shows around 333 only gives the car in the original door.
# firstwin column gives a cumulative count of all shows that got the car in the first door that remained unchanged.
# lastwin gives cuulative counts of wins if the audience always switched their choice of the door.
library(data.table)
library(magrittr)
library(purrr)
library(wrapr)

N <- 1000
possdoors <- combinat::permn(c("GOAT","GOAT","CAR"))

univ <- sample(possdoors,size = N,replace = T)


firstdoor <- sample(1:3,N,replace = T)

remdoors <- firstdoor %>% map(~setdiff(1:3,.x))

cardoors <- univ %>% map_int(~(.x=="CAR") %>% which)

seconddoor <- remdoors %>% map2_int(cardoors, ~ .x[which(.x != .y)][1])

thirddoor <- firstdoor %>% map2_int(seconddoor,~setdiff(1:3,c(.x,.y)))



dt <- univ %>% as.data.table() %>% t %>% as.data.table()
setnames(dt,qc(D1,D2,D3))

dt <- cbind(dt,data.table(First=firstdoor,Second=seconddoor,Remaining=thirddoor,firstwin=(firstdoor==cardoors),lastwin=(thirddoor==cardoors)))
dt[,firstwin:=cumsum(firstwin)][,lastwin:=cumsum(lastwin)]
print(dt)