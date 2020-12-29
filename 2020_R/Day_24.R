setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

load_data <- function(filepath){
        data <- readLines(filepath)
        return(data)
}

input <- load_data('day_24_data.txt')

eval_line <- function(line){
        x <- 0
        y <- 0
        chars <- strsplit(line,'')[[1]]
        idx <- 1
        while(idx<=length(chars)){
                ths <- chars[idx]
                if(ths=='e'){
                        x <- x+2
                        idx <- idx+1
                }else if(ths=='w'){
                        x <- x-2
                        idx <- idx+1
                }else if(ths=='n'){
                        y <- y+1
                        if(chars[idx+1]=='e'){
                                x <- x+1
                                idx <- idx+2
                        }else if(chars[idx+1]=='w'){
                                x <- x-1
                                idx <- idx+2
                        }else{
                                stop('this is not a valid instruction')
                        }
                }else if(ths=='s'){
                        y <- y-1
                        if(chars[idx+1]=='e'){
                                x <- x+1
                                idx <- idx+2
                        }else if(chars[idx+1]=='w'){
                                x <- x-1
                                idx <- idx+2
                        }else{
                                stop('this is not a valid instruction')
                        }
                }else{
                        stop('this is not a valid instruction')
                }
        }
        return(c(x,y))
        
}

parse_set <- function(input){
        on <- NULL
        for(i in 1:length(input)){
                loc <- paste(eval_line(input[i]),collapse=',')
                if(!loc %in% on){
                        on <- c(on,loc)
                }else{
                        on <- setdiff(on,loc)
                }
        }
        return(on)
}
print(length(parse_set(input)))

# part 2

init_brd <- function(input){
        ind <- matrix(ncol=2,nrow=length(input),data=0)
        for(i in 1:length(input)){
                ind[i,]<-eval_line(input[i])
                ind[i,2]<--ind[i,2]
        }
        minx <- min(ind[,1])
        ind[,1] <- ind[,1]-minx+1
        miny <- min(ind[,2])
        ind[,2] <- ind[,2]-miny+1
        maxx <- max(ind[,1])
        maxy <- max(ind[,2])
        board <- matrix(ncol=maxx,nrow=maxy,data=0)
        for(i in 1:nrow(ind)){
                board[ind[i,2],ind[i,1]] <- (board[ind[i,2],ind[i,1]]+1)%%2
        }
        return(board)
}

take_step <- function(board){
        new_board <- matrix(nrow=nrow(board)+8,ncol=ncol(board)+8,data=0)
        out_board <- matrix(nrow=nrow(board)+4,ncol=ncol(board)+4,data=0)
        new_board[5:(nrow(board)+4),5:(ncol(board)+4)]<-board
        parity <- min(which(board[,1]==1))%%2
        for(x in 3:(ncol(board)+6)){
                for(y in 3:(nrow(board)+6)){
                        if((x+y)%%2==parity){
                                next
                        }
                        val <- new_board[y,x]
                        neighs <- sum(new_board[(y-1):(y+1),(x-2):(x+2)])-val
                        if(val==1){
                                if(neighs %in% c(1,2)){
                                        out_board[y-2,x-2]<-1
                                }
                        }else{
                               if(neighs==2){
                                       out_board[y-2,x-2]<-1
                               } 
                        }
                }
        }
        full_rows <- which(rowSums(out_board)!=0)
        full_cols <- which(colSums(out_board)!=0)
        out_board <- out_board[min(full_rows):max(full_rows),min(full_cols):max(full_cols)]
        return(out_board)
}

run_game <- function(input,steps){
        brd <- init_brd(input)
        for(i in 1:steps){
                brd <- take_step(brd)
        }
        return(brd)
}
print(sum(run_game(input,100)))