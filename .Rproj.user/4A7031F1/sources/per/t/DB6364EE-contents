steps <- readLines('2022/stolte/puzzle09.txt',warn=F)
steps <- unlist(sapply(steps,function(s){
  times <- as.numeric(substring(s,3))
  return(rep(substring(s,1,1),times))
},USE.NAMES = F))

places <- list('1000010000'=T)
knots <- list(
  'H' = c(10000,10000)
  ,'1' = c(10000,10000)
  ,'2' = c(10000,10000)
  ,'3' = c(10000,10000)
  ,'4' = c(10000,10000)
  ,'5' = c(10000,10000)
  ,'6' = c(10000,10000)
  ,'7' = c(10000,10000)
  ,'8' = c(10000,10000)
  ,'9' = c(10000,10000)
)
lastKnot <- tail(names(knots),1)

directions = list('R' = c(1,0), 'L' = c(-1,0), 'U' = c(0,1), 'D' = c(0,-1))
sqrt5 <- sqrt(5)
sqrt8 <- sqrt(8)

for(step in steps){
  knots[[1]] <- knots[[1]] + directions[[step]]
  for(i in 2:length(knots)){
    a <- names(knots)[i-1]
    b <- names(knots)[i]
    dist <- sqrt((knots[[a]][1]-knots[[b]][1])^2+(knots[[a]][2]-knots[[b]][2])^2)
    if(dist == 2){
      knots[[b]] <- knots[[b]] + (knots[[a]]-knots[[b]])/2
    } else if(dist == sqrt5 | dist == sqrt8){
      knots[[b]] <- knots[[b]] + c(abs(knots[[a]][1]-knots[[b]][1])/(knots[[a]][1]-knots[[b]][1]),abs(knots[[a]][2]-knots[[b]][2])/(knots[[a]][2]-knots[[b]][2]))
    }
  }
  
  places[paste0(knots[[lastKnot]][1],knots[[lastKnot]][2])] <- T
  # print(knots)
  # invisible(readline("[ENTER] to continue"))
}

length(places)

