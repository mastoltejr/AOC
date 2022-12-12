lines <- c(" ",readLines('2022/stolte/puzzle11-test.txt', warn = F))
breaks <- c(1,which(lines ==""))
monkeys <- lapply(breaks, function(b) lines[(b+1):(b+6)])
interpretOperation <- function(s,divBy = 3) {
  symbol <- substring(s,24,24)
  num <- substring(s,26)
  if(num == 'old'){
    if(symbol == '+'){
      return(function(old) {
        return((old+old)) #%/%divBy)
      })
    }
    return(function(old){
      return((old*old)) #%/%divBy)
    })
  }
  num <- as.numeric(num)
  if(symbol == '+'){
    return(function(old) {
      return((old+num)) #%/%divBy)
    })
  }
  return(function(old){
    return((old*num)) #%/%divBy)
  })
}

interpretAction<- function(s,t,f) {
  num <- as.numeric(substring(s,22))
  t <- as.numeric(substring(t,nchar(t)-1))+1
  f <- as.numeric(substring(f,nchar(f)-1))+1
  return(function(val){
    if(is.finite(val) & is.integer(val/num)){ #val%%num==0
      monkeys[[t]]$items <<- c(monkeys[[t]]$items,val)
    } else {
      monkeys[[f]]$items <<- c(monkeys[[f]]$items,val)
    }
  })
}

monkeys <<- lapply(monkeys, function(m) list(name = m[1]
                                            ,items=as.numeric(unlist(strsplit(substring(m[2],19),', ',fixed=T)))
                                            ,operation = interpretOperation(m[3],1)
                                            ,action = interpretAction(m[4],m[5],m[6])
                                            ))


monkeyNames <- sapply(monkeys,function(m) m$name)
counts <- rep(0,length(monkeyNames))
names(counts) <- monkeyNames

i <- 1
# m <- 1
while(i <= 20){
  for(m in 1:length(monkeys)){
    monkey <- monkeys[[m]]
    items <- monkey$items
    monkeys[[m]]$items <- numeric()
    # item <- items[1]
    for(item in items){
      counts[monkey$name] <- counts[monkey$name] + 1
      item <- monkey$operation(item)
      monkey$action(item)
    }
  }
  
  i <- i + 1
}

prod(tail(sort(counts),2))
