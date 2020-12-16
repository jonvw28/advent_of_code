# initially I ran this with functional namescoping, but to save it getting out
# of hand I used enforced global scoping to avoid wasting time simply copying
# data, most of which is not needed

setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

init_board <- function(data,turns){
        board <<- integer(length=turns+1)
        board[data[1]+1]<<- 1
        for(i in 1:length(data)){
                board[data[i]+1]<<-i
        }
        return(0)
}
               
take_turn <- function(this,turn){
        if(board[this+1]==0){
                nxt <- 0
        }else{
                nxt <- turn - board[this+1]
        }
        board[this+1]<<- turn
        return(nxt)
}

 run_game <- function(data,turns){
         res <- init_board(data,turns)
         for(turn in (length(data)+1):(turns-1)){
                 res <- take_turn(res,turn)
                 if(turn%%1000000==0){
                         print(turn)
                 }
         }
         return(res)
}

data <- as.numeric(read.csv('day_15_data.txt',header = F)[1,]) 
print(run_game(data,2020)) 
print(run_game(data,30000000)) 

