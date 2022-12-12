data <- as.numeric(readLines('2021/stolte/puzzle01.txt',warn=F))
shifted <- c(NA, head(data,-1))
sum(data > shifted, na.rm = T)

sum3 <- sapply(1:(length(data)-2), function(i) sum(data[i:(i+2)]))
shifted <- c(NA, head(sum3,-1))
sum(sum3 > shifted, na.rm = T)
