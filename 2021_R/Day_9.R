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

find_lows <- function(board){
        padBoard <- array(data=100,dim=dim(board)+2)
        padBoard[2:(dim(board)[1]+1),2:(dim(board)[2]+1)]<-board
        resBoard <- array(data=FALSE,dim=dim(board))
        for(i in 1:nrow(board)){
                for(j in 1:ncol(board)){
                         neigh <- c(padBoard[i,j+1],
                                    padBoard[i+1,j],
                                    padBoard[i+2,j+1],
                                    padBoard[i+1,j+2])
                         if(board[i,j] < min(neigh)){
                                 resBoard[i,j]<-TRUE
                         }
                }
        }
        return(resBoard)
}

board <- data_parser("day_9_data.txt")
sum(board[find_lows(board)]+1)

label_lows <- function(board){
        padBoard <- array(data=100,dim=dim(board)+2)
        padBoard[2:(dim(board)[1]+1),2:(dim(board)[2]+1)]<-board
        resBoard <- array(data=0,dim=dim(board))
        tag <-1
        for(i in 1:nrow(board)){
                for(j in 1:ncol(board)){
                        neigh <- c(padBoard[i,j+1],
                                   padBoard[i+1,j],
                                   padBoard[i+2,j+1],
                                   padBoard[i+1,j+2])
                        if(board[i,j] < min(neigh)){
                                resBoard[i,j] <- tag
                                tag <- tag+1
                        }
                }
        }
        return(resBoard)
}

build_basins <- function(board,lows){
        padLow <- array(data=0,dim=dim(lows)+2)
        padLow[2:(dim(lows)[1]+1),2:(dim(lows)[2]+1)]<-lows
        #skip 0 as automatically labelled by low finder
        for(currNo in 1:8){
                for(i in 1:nrow(lows)){
                        for(j in 1:ncol(lows)){
                                if(board[i,j]!=currNo){
                                        next
                                }
                                if(padLow[i+1,j+1]!=0){
                                        next
                                }
                                neigh <- c(padLow[i,j+1],
                                           padLow[i+1,j],
                                           padLow[i+2,j+1],
                                           padLow[i+1,j+2])
                                if(length(unique(neigh))>2){
                                        stop("Uh Oh your logic is flawed consdier a less brtue force algorithm")
                                }
                                padLow[i+1,j+1]<-max(neigh)
                        }
                }
        }
        return(padLow[2:(dim(lows)[1]+1),2:(dim(lows)[2]+1)])
}

part2 <- function(basins){
        numB <- max(basins)
        currSize <- c(0,0,0)
        for(i in 1:numB){
                size <- sum(basins==i)
                if(size>currSize[3]){
                        currSize[3] <- size
                        currSize <- sort(currSize,decreasing = TRUE)
                }
        }
        return(prod(currSize))
}

part2(build_basins(board,label_lows(board)))