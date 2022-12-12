rm(list=ls());cat("\014");suppressWarnings(gc())
library(data.table)

text <- readLines('stolte/puzzle06.txt', warn=F)
text <- unlist(strsplit(text,""))

packet_marker <- sapply(4:length(text), function(i) anyDuplicated(text[(i-3):i]) == 0)
names(packet_marker) <- 4:length(text)
packet_marker[head(which((packet_marker)),1)]

message_marker <- sapply(14:length(text), function(i) anyDuplicated(text[(i-13):i]) == 0)
names(message_marker) <- 14:length(text)
message_marker[head(which((message_marker)),1)]
