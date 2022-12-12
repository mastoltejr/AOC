trees <- readLines('stolte/puzzle08.txt',warn=F)
trees <- t(sapply(trees,function(x) as.numeric(unlist(strsplit(x,split=""))),USE.NAMES = F))
width <- dim(trees)[1]
height <- dim(trees)[2]

findMax <- function(line, rev = F) {
  N <- length(line)
  if(rev) return(c(sapply(1:(N-1),function(i) max(line[(i+1):N])),-1))
  c(-1,sapply(2:N,function(i) max(line[1:(i-1)])))
}

fromLeft <- t(sapply(1:width,function(i) findMax(trees[i,])))
fromRight <- t(sapply(1:width, function(i) findMax(trees[i,],rev = T)))
fromTop <- sapply(1:height,function(i) findMax(trees[,i]))
fromBottom <- sapply(1:height, function(i) findMax(trees[,i],rev = T))

counts <- matrix(nrow=height, ncol = width)
for(i in 1:width){
  for(j in 1:height){
    counts[j,i] <- sum(trees[j,i] > c(fromLeft[j,i],fromRight[j,i],fromTop[j,i],fromBottom[j,i]))
  }
}

sum(counts >= 1)

# Part 2

findCumMax <- function(line, rev = F) {
  N <- length(line)
  if(rev){
    return(c(sapply(1:(N-1),function(i){
      view <- cummax(line[(i+1):N])
      sum(line[i] > view)+!all(view < line[i]) # i.e. add one if it ran into something bigger or the edge
    }),0))
  }
  c(0,sapply(2:N,function(i) {
    view <- cummax(line[(i-1):1])
    sum(line[i] > view)+!all(view < line[i]) # i.e. add one if it ran into something bigger or the edge
  }))
}

lookingLeft <- t(sapply(1:width,function(i) findCumMax(trees[i,])))
lookingRight <- t(sapply(1:width, function(i) findCumMax(trees[i,],rev = T)))
lookingUp <- sapply(1:height,function(i) findCumMax(trees[,i]))
lookingDown <- sapply(1:height, function(i) findCumMax(trees[,i],rev = T))

counts2 <- matrix(nrow=height, ncol = width)
for(i in 1:width){
  for(j in 1:height){
    counts2[j,i] <- prod(c(lookingLeft[j,i],lookingRight[j,i],lookingUp[j,i],lookingDown[j,i]))
  }
}

max(counts2)

