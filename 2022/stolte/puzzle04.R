rm(list=ls());cat("\014");suppressWarnings(gc())
library(data.table)

# data <- c(
#   "2-4,6-8",
#   "2-3,4-5",
#   "5-7,7-9",
#   "2-8,3-7",
#   "6-6,4-6",
#   "2-6,4-8"
# )

data <- readLines('stolte/puzzle04.txt')
data <- data.table(read.table(text=gsub("-",",",data,fixed=T),sep=","))
names(data) <- c('Start1','End1','Start2','End2')
areContained <- function(a,b,x,y) a <= x & b >= y

data[, isContained := areContained(Start1, End1, Start2, End2) |
       areContained(Start2, End2, Start1, End1), ]

nrow(data[(isContained),])

areOverlapping <- function(a,b,x,y) a <= x & b >= x

data[, isOverlapping := areOverlapping(Start1, End1, Start2, End2) |
       areOverlapping(Start2, End2, Start1, End1), ]

nrow(data[(isOverlapping),])
