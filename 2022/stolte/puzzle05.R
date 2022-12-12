rm(list=ls());cat("\014");suppressWarnings(gc())
library(data.table)

lines <- readLines('stolte/puzzle05.txt', warn=F)
blank <- which(lines == '')
crate_indices <- which(unlist(strsplit(lines[blank-1],"")) != " ")
crates <<- as.data.table(strsplit(lines[(blank-1):1],""))[crate_indices,]
crates <<- lapply(1:(blank-1), function(i) {
  stack <- tail(as.character(crates[i,]),-1)
  stack[stack != " "]
})

movements <- regmatches(tail(lines,-blank), gregexpr("[[:digit:]]+", tail(lines,-blank)))
N <- length(movements)

move <- function(movement, step=1, rev = T){
  quantity <- as.numeric(movement[[1]])
  from <- as.numeric(movement[[2]])
  to <- as.numeric(movement[[3]])
  
  print(paste0(step,'/',N))
  onCrane <- tail(crates[[from]],quantity)
  if(rev) onCrane <- rev(onCrane)
  crates[[from]] <<- head(crates[[from]],-quantity)
  crates[[to]] <<- c(crates[[to]],onCrane)
}

for(i in 1:N){
  move(movements[[i]],i, rev=F)
}

crates
sapply(crates,tail,1)
