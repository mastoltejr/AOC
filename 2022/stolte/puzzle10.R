signals <- readLines('puzzle10.txt',warn=F)
signals <- c(1,unlist(sapply(signals, function(signal) {
  if(signal == 'noop') return(0)
  return(c('x' = 0, 'y' = as.numeric(substring(signal,6))))
})))
signals <- cumsum(signals)
names(signals) <- 1:length(signals)
indicies <- c(20,60,100,140,180,220)
sum(indicies*signals[indicies])

drawnPixels <- which(abs(signals[1:240]-rep(0:39,6)) <= 1)
img <- rep('.',241)
img[drawnPixels] <- '#'

for(i in c(1,41,81,121,161,201)){
  print(paste(img[i:(i+39)],collapse=""))
}
