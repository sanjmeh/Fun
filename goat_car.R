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