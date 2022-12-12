rm(list=ls());cat("\014");suppressWarnings(gc())
library(data.table)

data <- data.table(read.table('stolte/puzzle02.txt')) 
names(data) <- c('Opponent','You')

results <- data.table(
  Player1 = rep(c('A','B','C'),times=3),
  Player2 = rep(c('X','Y','Z'), each=3),
  PT1 = rep(c(1,2,3),each=3),
  PT2 = c(3,0,6,6,3,0,0,6,3)
)

data[results, c('PT1','PT2') := list(i.PT1,i.PT2) , on = c(Opponent='Player1',You='Player2')]
data[,Points := PT1 + PT2]

# Part 1 Answer
sum(data$Points)

# Part 2

gameplay <- data.table(
  Player1 = rep(c('A','B','C'),times=3),
  Result = rep(c('X','Y','Z'), each=3),
  Player2 = c('Z','X','Y','X','Y','Z','Y','Z','X')
)

data[gameplay,NeedToPlay := i.Player2, on = c(Opponent='Player1',You='Result')]
data[results, c('PT2.1','PT2.2') := list(i.PT1,i.PT2) , on = c(Opponent='Player1',NeedToPlay='Player2')]
data[,Points2 := PT2.1 + PT2.2]

# Part 2 Answer
sum(data$Points2)
