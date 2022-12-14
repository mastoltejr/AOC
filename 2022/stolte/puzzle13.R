rm(list=ls());cat("\014");suppressWarnings(gc())
lines <- c("",readLines('2022/stolte/puzzle13-test.txt',warn=F))
breaks <- which(lines =="")
groups <- lapply(breaks, function(b) lines[(b+1):(b+2)])


interpretList <- function(s){
  s <- head(tail(s,-1),-1)
  l <- list()
  i <- 1
  while(i <= length(s)){
    c <- s[i]
    if(!suppressWarnings(is.na(as.numeric(c)))) {
      l <- c(l,as.numeric(c))
    } else if(c == '['){
      closingBracket <- which(cumsum(s == '[') - cumsum(s == ']')==0)
      closingBracket <- head(closingBracket[closingBracket > i],1)
      l <- c(l,list(interpretList(s[i:closingBracket])))
      i <- i+closingBracket-1
    } 
    i <- i + 1
  }
  if(length(l) == 0) return(list(numeric()))
  return(l)
}

s <- '[[4,[[]]],[],[[1]],[]]'
s <- unlist(strsplit(s,""))
x <- interpretList(s)
