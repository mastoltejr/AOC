rm(list=ls());cat("\014");suppressWarnings(gc())
cracks <- readLines('2022/stolte/puzzle14.txt',warn=F)
addFloor <- T
cracks <- lapply(cracks, function(crack) unlist(strsplit(crack, ' -> ')))
crackMap <<- list()
printMatrix <- function(mat){
  for(i in 1:dim(mat)[1]){
    print(paste(mat[i,],collapse=''))
  }
}

for(crack in cracks){
  for(i in 2:length(crack)){
    a <- as.numeric(unlist(strsplit(crack[i-1], ',')))
    b <- as.numeric(unlist(strsplit(crack[i],',')))
    
    N <- max(abs(c(a[1]-b[1], a[2]-b[2])))+1
    cols <- seq(from=a[1], to=b[1], length.out=N)
    rows <- as.character(seq(from=a[2], to=b[2], length.out=N))
    
    for(j in 1:N){
      if(is.null(crackMap[[rows[j]]])){
        crackMap[[rows[j]]] <- cols[j]
      } else {
        crackMap[[rows[j]]] <- c(crackMap[[rows[j]]], cols[j])
      }
    }
  }
}

caveDepth <- max(as.numeric(names(crackMap)))
caveWidth <- unlist(crackMap)
caveWidth <- c(min(caveWidth), max(caveWidth))
mapOffset <- caveDepth
caveWidth <- caveWidth + mapOffset*c(-1,1) + c(0,2)
cave <- matrix('.', nrow=caveDepth+4, ncol=caveWidth[2]-caveWidth[1]+1)
crackMap <- lapply(crackMap,`-`,(caveWidth[1]-1))
leakPosition <- c(1,500-caveWidth[1]+1)
cave[leakPosition[1], leakPosition[2]] <- '+'
caveWidth <- caveWidth[2]-caveWidth[1]+1

if(addFloor){
  cave[caveDepth + 2 + 1,] <- rep('#',caveWidth)
  caveDepth <- caveDepth + 2 + 1
}

for(row in names(crackMap)){
  cave[as.numeric(row) + 1, crackMap[[row]]] <- '#'
}

moves <- list('Down' = c(1,0), 'Down-Left' = c(1,-1), 'DownRight' = c(1,1))

position <- leakPosition

printMatrix(cave)

sandCount <- 1
while(position[1] < caveDepth){
  options <- which(sapply(moves, function(x) {
    dropTo <- position+x
    if(dropTo[2] <= 0 | dropTo[2] > caveWidth) return(NA)
    cave[dropTo[1], dropTo[2]]
  })=='.')
  
  if(length(options) == 0){
    sandCount <- sandCount + 1
    print(sandCount)
    if(all(position == leakPosition)){
      break
    }
    position <- leakPosition
  } else {
    cave[position[1],position[2]] <- '.'
    move <- moves[[head(options,1)]]
    position <- position + move
    cave[position[1],position[2]] <- 'o'
    cave[leakPosition[1],leakPosition[2]] <- '+'
    # print('==============================')
    # printMatrix(cave)
  }
  
  # invisible(readline('[ENTER]'))
}

printMatrix(cave)
print(sandCount-1)





