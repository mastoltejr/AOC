

# Overall Strategy --------------------------------------------------------

# Split elve's counts into groups by elf
# Sum each group
# Find max by sorting then taking the end value

# data <- c(1000,2000,3000,NA,4000,NA,5000,6000,NA,7000,8000,9000,NA,10000) # Test Data

data <- as.integer(c(readLines('stolte/puzzle01.txt'),"")) # Added a tailing NA to capture end of last group

split_indices <- which(is.na(data)) # Where are the NA or blank lines ?
split_indices <- split_indices - c(0,head(split_indices,-1)) - 1 # Find the # of items between blank lines by subtracting previous index of NA, 
                                                                 # subtract 1 to get rid of NAs

split_groups <- rep(seq_along(split_indices),times=split_indices) # For each group, repeat that group number "length of group" times
elves <- split(na.omit(data), split_groups) # split data into groups, remove NAs first

# Part 1
tail(sort(sapply(elves, sum)),1) # apply sum to each group, sort the result, grab the last item in the sorted list

# Part 2
sum(tail(sort(sapply(elves, sum)),3)) # apply sum to each group, sort the result, grab last 3 items in the sorted list, sum those items
