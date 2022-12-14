rm(list=ls());cat("\014");suppressWarnings(gc())
data <- as.matrix(sapply(readLines('2022/stolte/puzzle12.txt',warn=F),function(s) unlist(strsplit(s,''))))
dimensions <- dim(data)
toXY <- function(x) c(x%%dimensions[1],x%/%dimensions[1]+1)
toList <- function(x) (x[2]-1)*dimensions[1]+x[1]

start <- which(data == 'S')
end <- which(data == 'E')
heights <- apply(data,c(1,2),function(l) match(l,letters))
heights[start] <- 1
heights[end] <- 26

cardinal <- c(-1,-dimensions[1],1,dimensions[1])

# findNextOptions <- function(path){
#   spot <- tail(path,1)
#   options <- spot + cardinal
#   options <- options[options > 0 & options <= length(heights)]
#   options <- options[!options %in% path]
#   options <- options[heights[options]-heights[spot] <= 1]
#   options
# }
# 
# 
# paths <- list(start)
# spots <<- list()
# j <- 0
# while(!any(sapply(paths,tail,1) == end)){
#   
#   if(length(paths) > 1){
#     l <- sapply(paths, function(p){
#       if(!is.null(spots[[as.character(tail(p,1))]])) return(F)
#       spots[[as.character(tail(p,1))]] <<- length(p)
#       return(T)
#     })
#     paths <- paths[l]
#   }
#   
#   paths <- do.call(c,lapply(paths, function(path){
#     options <- findNextOptions(path)
#     lapply(options, function(o) c(path, o))
#   }))
#   
#  print(paste0(length(paths),' path(s) size ',j))
#  j <- j + 1
#  # invisible(readline('[ENTER]'))
# }
# 
# length(paths[[1]])-1

# Part 2

findNextOptions <- function(path){
  spot <- tail(path,1)
  options <- spot + cardinal
  options <- options[options > 0 & options <= length(heights)]
  options <- options[!options %in% path]
  options <- options[heights[spot]-heights[options] <= 1]
  options
}


paths <- list(end)
spots <<- list()
j <- 0
while(!any(heights[sapply(paths,tail,1)] == 1)){
  
  if(length(paths) > 1){
    l <- sapply(paths, function(p){
      if(!is.null(spots[[as.character(tail(p,1))]])) return(F)
      spots[[as.character(tail(p,1))]] <<- length(p)
      return(T)
    })
    paths <- paths[l]
  }
  
  paths <- do.call(c,lapply(paths, function(path){
    options <- findNextOptions(path)
    lapply(options, function(o) c(path, o))
  }))
  
  print(paste0(length(paths),' path(s) size ',j))
  j <- j + 1
  # invisible(readline('[ENTER]'))
}

length(paths[[1]])-1

