setwd("D:/jon/Documents/Cache/advent_of_code/2021_R")


data_parser <- function(connection){
        lines <- readLines(connection)
        board <- array(data=-1,dim=c(length(lines),nchar(lines[1])))
        vals <- strsplit(lines,'')
        for(i in 1:length(lines)){
                for (j in 1:length(vals[[1]])){
                        board[i,j] <- as.numeric(vals[[i]][j])
                }
        }
        return(board)
}

take_step <- function(data){
        flashes <- array(data=0,dim=dim(data))
        data <- data + 1
        flashes[data>9 & flashes==0] <- 1
        while(sum(flashes==1)!=0){
                for(loc in which(flashes==1)){
                        c <- ceiling(loc/10)
                        r <- loc - (c-1)*10
                        data[max(1,r-1):min(10,r+1),max(1,c-1):min(10,c+1)] <- data[max(1,r-1):min(10,r+1),max(1,c-1):min(10,c+1)]+1
                        flashes[loc] <- 2
                }
                flashes[data>9 & flashes==0] <- 1
        }
        data[data>9] <- 0
        return(list(sum(flashes!=0),data))
}

run_steps <- function(data,steps){
        flashes <- 0
        board <- data
        for(i in 1:steps){
                res <- take_step(board)
                flashes <- flashes + res[[1]]
                board <- res[[2]]
        }
        return(list(flashes,board))
}

data <- data_parser("day_11_data.txt")
run_steps(data,100)[[1]]

find_synchro <- function(data){
        ticker <- 0
        out <- 0
        board <- data
        while(out!=100){
                res <- take_step(board)
                out <- res[[1]]
                board <- res[[2]]
                ticker <- ticker + 1
        }
        return(ticker)
}

find_synchro(data)