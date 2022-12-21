setwd("D:/jon/Documents/Cache/advent_of_code/2021_R")

data_parser <- function(connection){
        data <- readLines(connection)
        data <- as.numeric(strsplit(data,",")[[1]])
        data <- factor(data,levels=0:8)
        return(as.numeric(table(data)))
}

move_day <- function(data){
        newData <- numeric(length=9)
        newData[1:8]<- data[2:9]
        newData[9] <- data[1]
        newData[7] <- newData[7]+data[1]
        return(newData)
}

run_period <- function(data,days){
        for(i in 1:days){
                data <- move_day(data)
        }
        return(data)
}

data <- data_parser("day_6_data.txt")
sum(run_period(data,80))
print(sum(run_period(data,256)),digits=22)
