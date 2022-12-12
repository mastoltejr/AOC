rm(list=ls());cat("\014");suppressWarnings(gc())

data <- readLines('stolte/puzzle03.txt')
findDuplicate <- function(s) {
  N <- nchar(s)
  c1 <- unlist(strsplit(substring(s,1,N/2),""))
  c2 <- unlist(strsplit(substring(s,N/2+1,N),""))
  head(c1[c1 %in% c2],1)
}

duplicates <- sapply(data, findDuplicate)
map <- 1:52
names(map) <- c(letters,LETTERS)
sum(map[duplicates])

# Part 2

groups <- split(data,rep(1:(length(data)/3),each=3))
findCommonality <- function(g) {
  items <- strsplit(g,"")
  common <- items[[1]][items[[1]] %in% items[[2]]]
  head(common[common %in% items[[3]]],1)
}
badges <- sapply(groups, findCommonality)
sum(map[badges])
