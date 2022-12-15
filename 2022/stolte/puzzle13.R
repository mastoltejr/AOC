rm(list=ls());cat("\014");suppressWarnings(gc())
lines <- c("",readLines('2022/stolte/puzzle13.txt',warn=F))
breaks <- which(lines =="")
strings <- lapply(breaks, function(b) lines[(b+1):(b+2)])

splitString <- function(s){
  s <- unlist(strsplit(s,''))
  r <- character()
  i <- 1
  while(i <= length(s)){
    j <- i
    while(suppressWarnings(all(!is.na(as.numeric(s[i:(j+1)]))))){
      j <- j + 1
    }
    r <- c(r,paste(s[i:j],collapse=""))
    i <- j + 1
  }
  return(r)
}

stringGroups <- lapply(strings, function(l){
  list(splitString(l[1]),splitString(l[2]))
})


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
      i <- closingBracket
    }
    i <- i + 1
  }
  if(length(l) == 0) return(list(numeric()))
  return(l)
}

listGroups <- lapply(stringGroups, function(g){
  list(left = interpretList(g[[1]]), right = interpretList(g[[2]]))
})

compareLists <- function(left, right){
  if(length(left) == 0) return(TRUE)

  for(i in 1:length(left)){
    if(i > length(right)) return(FALSE)
    
    if(is.numeric(left[[i]]) & is.numeric(right[[i]])){
      
      if(length(left[[i]]) == 0 & length(right[[i]]) == 0) next
      if(length(left[[i]]) == 0) return(TRUE)
      if(length(right[[i]]) == 0) return(FALSE)
      if(left[[i]] == right[[i]]) next
      return(left[[i]] < right[[i]])
      
    } else if(is.numeric(left[[i]]) & is.list(right[[i]])){
      
      if(length(left[[i]]) == 0) return(TRUE)
      return(compareLists(list(left[[i]]), right[[i]]))
      
    } else if(is.list(left[[i]]) & is.numeric(right[[i]])){
      
      if(length(right[[i]]) == 0) return(FALSE)
      return(compareLists(left[[i]], list(right[[i]])))
      
    } else {
      return(compareLists(left[[i]], right[[i]]))
    }
  }
  
  return(TRUE)
}


for(i in 1:length(listGroups)){
  print('==================================')
  print(i)
  left <- listGroups[[i]]$left
  right <- listGroups[[i]]$right
  print(strings[[i]][1])
  print(strings[[i]][2])
  print(compareLists(left, right))
  invisible(readline('[ENTER]'))
}


results <- sapply(listGroups, function(g){
  left <- g$left
  right <- g$right
  # compareLists(left, right)
  return(compareLists(left, right))
})

sum(which(results))
