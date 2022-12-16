setwd("D:/jon/Documents/Cache/advent_of_code/2021_R")

count_increases <- function(log){
        change <- diff(log)
        return(sum(change>0))
}

data <- read.csv('day_1_data.txt',header=F)
count_increases(data[,1])

# part 2

count_3_increases <- function(log){
        a <- log[1:(length(log)-2)]
        b <- log[2:(length(log)-1)]
        c <- log[3:(length(log))]
        return(count_increases(a+b+c))
}
count_3_increases(data[,1])