rm(list=ls());cat("\014");suppressWarnings(gc())
commands <- readLines('stolte/puzzle07-kamren.txt',warn=F)
fileSystem <- list()
path <- character()
pathStr <- ''

bubbleUp <- function(dir,size) {
  if(length(dir) <= 1) return()
  dir <- head(dir,-1)
  tempPath <- paste(dir,collapse="_/_")
  fileSystem[[tempPath]]$size <<- fileSystem[[tempPath]]$size + size
  bubbleUp(dir,size)
}
i <- 1
N <- length(commands)
for(command in commands){
  print(paste0(command,' | ',i,'/',N))
  if(command == '$ cd ..'){
    path <- head(path,-1)
    pathStr <- paste(path,collapse="_/_")
    
  } else if(command == '$ cd /'){
    path <- c('/')
    pathStr <- '/'
    if(is.null(fileSystem[[pathStr]])) fileSystem[[pathStr]] <- list(path=pathStr,dirs = character(), files=numeric(), size=0)
    
  } else if(startsWith(command,'$ cd ')){
    dir <- substring(command,first=6)
    if(pathStr != '') fileSystem[[pathStr]]$dirs <- unique(c(fileSystem[[pathStr]]$dirs,dir))
    path <- c(path,dir)
    pathStr <- paste(path, collapse="_/_")
    if(is.null(fileSystem[[pathStr]])) fileSystem[[pathStr]] <- list(path=pathStr,dirs = character(), files=numeric(), size=0)
    
  } else if(startsWith(command,'dir')) {
    dir <- substring(command,first=5)
    fileSystem[[pathStr]]$dirs <- c(fileSystem[[pathStr]]$dirs,dir)
    newDirPath <- paste(c(path,dir),collapse="_/_")
    if(is.null(fileSystem[[newDirPath]])) fileSystem[[newDirPath]] <- list(path=newDirPath,dirs = character(), files=numeric(), size=0)
  } else if(command != '$ ls'){
    command <- unlist(strsplit(command,' '))
    if(!command[2] %in% names(fileSystem[[pathStr]]$files)){
      fileSystem[[pathStr]]$files[command[2]] <- as.numeric(command[1])
      fileSystem[[pathStr]]$size <- fileSystem[[pathStr]]$size + as.numeric(command[1])
      bubbleUp(path,as.numeric(command[1]))
    }
  }
  # print(pathStr)
  i <- i + 1
  # invisible(readline(prompt="Press [enter] to continue"))
}

part1Directories <- sapply(fileSystem, function(x) ifelse(x$size <= 100000,x$size,0))
sum(part1Directories)

total_disk_size <- 70000000
space_needed <- 30000000
used_space <- fileSystem[['/']]$size
min_space_to_delete <- space_needed - (total_disk_size - used_space)

part2Directories <- sapply(fileSystem, function(x) ifelse(x$size >= min_space_to_delete,x$size,Inf))
min(part2Directories)


