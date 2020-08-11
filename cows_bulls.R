# Aug 11 - fun programming. New package: gtools
# Cows and Bulls Game. The program will guess your string.
library(data.table)
library(magrittr)
library(purrr)
library(wrapr)

N <- 6
M <- 4

#secret <- sample(seq_len(N),size = M)

mat1 <- gtools::permutations(n = N,r = M,v = LETTERS[seq_len(N)])



bulls <- function(seq1,seq2){
        (seq1==seq2) %>% sum
}

bulls_which <- function(seq1,seq2){
        (seq1==seq2) %>% which
}

cows <- function(seq1,seq2){
        rem <- bulls_which(seq1,seq2)
        if(length(rem)>0){
                seq1 <- seq1[-rem]
                seq2 <- seq2[-rem]
        }
        if(length(seq1) != length(seq2)) browser()
        x <- seq1  %>%  map_lgl(~.x %in% seq2 %>% any) %>% sum
        return(x)
}

i<- 1
while(length(mat1)>M & i<10){
        #cat("secret:",secret,"\n")
        guess <- mat1[sample(seq_len(nrow(mat1)),1),seq_len(M)]
        print(guess)
        x <- readline("Enter bulls: ")
        y <- readline("Enter cows: ")
        i<- i+ 1
        colcows <- apply(mat1[,seq_len(M)],1,cows,seq2=guess)
        colbulls <- apply(mat1[,seq_len(M)],1,bulls,seq2=guess)
        mat1 <- mat1[colbulls==x & colcows==y,]
        if(length(mat1)==M) message("Yay!! I got it!") else 
                if(length(mat1)==0) message("Hmm.. you made some mistake in clues :-( ")
        print(mat1)
}