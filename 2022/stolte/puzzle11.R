# rm(list=ls());cat("\014");suppressWarnings(gc())
# lines <- c(" ",readLines('2022/stolte/puzzle11.txt', warn = F))
# breaks <- c(1,which(lines ==""))
# monkeys <- lapply(breaks, function(b) lines[(b+1):(b+6)])
# interpretOperation <- function(s,divBy=1) {
#   symbol <- substring(s,24,24)
#   num <- substring(s,26)
#   if(num == 'old'){
#     if(symbol == '+'){
#       return(function(old) {
#         return((old+old)%/%divBy)
#       })
#     }
#     return(function(old){
#       return((old*old)%/%divBy)
#     })
#   }
#   num <- as.numeric(num)
#   if(symbol == '+'){
#     return(function(old) {
#       return((old+num)%/%divBy)
#     })
#   }
#   return(function(old){
#     return((old*num)%/%divBy)
#   })
# }
# 
# interpretAction<- function(s,t,f) {
#   num <- as.numeric(substring(s,22))
#   t <- as.numeric(substring(t,nchar(t)-1))+1
#   f <- as.numeric(substring(f,nchar(f)-1))+1
#   return(function(val){
#     if(val%%num==0){
#       # print(paste0(val,' divisible by ',num, 'pass',val,' to ', t))
#       monkeys[[t]]$items <<- c(monkeys[[t]]$items,val)
#     } else {
#       # print(paste0('pass ',val,' to ', f))
#       monkeys[[f]]$items <<- c(monkeys[[f]]$items,val)
#     }
#   })
# }
# 
# monkeys <<- lapply(monkeys, function(m) list(name = m[1]
#                                             ,items=as.numeric(unlist(strsplit(substring(m[2],19),', ',fixed=T)))
#                                             ,operation = interpretOperation(m[3],3)
#                                             ,action = interpretAction(m[4],m[5],m[6])
#                                             ))
# 
# 
# monkeyNames <- sapply(monkeys,function(m) m$name)
# counts <- rep(0,length(monkeyNames))
# names(counts) <- monkeyNames
# 
# i <- 1
# # m <- 1
# while(i <= 20){
#   for(m in 1:length(monkeys)){
#     monkey <- monkeys[[m]]
#     items <- monkey$items
#     monkeys[[m]]$items <- numeric()
#     # item <- items[1]
#     for(item in items){
#       counts[monkey$name] <- counts[monkey$name] + 1
#       item <- monkey$operation(item)
#       monkey$action(item)
#     }
#   }
#   print(paste0('============== ',i,' ===================='))
#   print(counts)
#   
#   i <- i + 1
# }
# 
# prod(tail(sort(counts),2))


# part 2

rm(list=ls());cat("\014");suppressWarnings(gc())
lines <- c(" ",readLines('2022/stolte/puzzle11.txt', warn = F))
breaks <- c(1,which(lines ==""))
monkeys <- lapply(breaks, function(b) lines[(b+1):(b+6)])

itemMap <<- numeric()
divisors <<- numeric()

interpretOperation <- function(s) {
  symbol <- substring(s,24,24)
  num <- substring(s,26)
  if(num == 'old'){
    if(symbol == '+'){
      return(function(old) {
        itemMap[[old]] <<- (itemMap[[old]]+itemMap[[old]])%%divisors
      })
    }
    return(function(old){
      itemMap[[old]] <<- (itemMap[[old]]*itemMap[[old]])%%divisors
    })
  }
  num <- as.numeric(num)
  if(symbol == '+'){
    return(function(old) {
      itemMap[[old]] <<- (itemMap[[old]]+num)%%divisors
    })
  }
  return(function(old){
    itemMap[[old]] <<- (itemMap[[old]]*num)%%divisors
  })
}

interpretAction<- function(s,t,f) {
  divisor <- as.numeric(substring(s,22))
  dIndex <- which(divisors == divisor)
  t <- as.numeric(substring(t,nchar(t)-1))+1
  f <- as.numeric(substring(f,nchar(f)-1))+1
  return(function(val){
    if(itemMap[[val]][dIndex]==0){
      # print(paste0(val,' divisible by ',num, 'pass',val,' to ', t))
      monkeys[[t]]$items <<- c(monkeys[[t]]$items,val)
    } else {
      # print(paste0('pass ',val,' to ', f))
      monkeys[[f]]$items <<- c(monkeys[[f]]$items,val)
    }
  })
}

options <- c(letters,LETTERS)

monkeys <<- lapply(monkeys, function(m) {
  
  items <- as.numeric(unlist(strsplit(substring(m[2],19),', ',fixed=T)))
  itemMap <<- c(itemMap,items)
  divisors <<- c(divisors,as.numeric(substring(m[4],22)))
  
  list(name = m[1]
      ,items=options[(length(itemMap)-length(items)+1):length(itemMap)]
      ,operation = interpretOperation(m[3])
      ,action = interpretAction(m[4],m[5],m[6])
  )
})

itemMap <<- setNames(lapply(itemMap, function(i) sapply(divisors, function(d) i%%d)),options[1:length(itemMap)])

monkeyNames <- sapply(monkeys,function(m) m$name)
counts <- rep(0,length(monkeyNames))
names(counts) <- monkeyNames

i <- 1
# m <- 1
while(i <= 10000){
  for(m in 1:length(monkeys)){
    monkey <- monkeys[[m]]
    items <- monkey$items
    monkeys[[m]]$items <- character()
    # item <- items[1]
    for(item in items){
      counts[monkey$name] <- counts[monkey$name] + 1
      monkey$operation(item)
      monkey$action(item)
    }
  }
  # print(paste0('============== ',i,' ===================='))
  # print(counts)

  i <- i + 1
}

prod(tail(sort(counts),2))





