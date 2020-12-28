setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

parse_input <- function(filepath){
        data <- readLines(filepath)
        split <- which(data=='')
        decks <- list()
        decks[[1]] <- as.numeric(data[2:(split-1)])
        decks[[2]] <- as.numeric(data[(split+2):length(data)])
        return(decks)
}

decks <- parse_input('day_22_data.txt')
#decks <- parse_input('tmp_data.txt')
#decks <- parse_input('tmp_2.txt')


take_turns <- function(decks,count){
        lim <- min(length(decks[[1]]),length(decks[[2]]))
        tmp_1 <- decks[[1]][1:lim]
        tmp_2 <- decks[[2]][1:lim]
        win_1 <- tmp_1>tmp_2
        deck1 <- c(rbind(tmp_1[win_1],tmp_2[win_1]))
        deck2 <- c(rbind(tmp_2[!win_1],tmp_1[!win_1]))
        if(length(decks[[1]])>lim){
                deck1 <- c(decks[[1]][(lim+1):length(decks[[1]])],deck1)
        }
        if(length(decks[[2]])>lim){
                deck2 <- c(decks[[2]][(lim+1):length(decks[[2]])],deck2)
        }
        return(list(list(deck1,deck2),count+lim))        
}

play_game <- function(decks){
        count <- 0
        while((length(decks[[1]])!=0)&(length(decks[[2]])!=0)){
                tmp <- take_turns(decks,count)
                decks <- tmp[[1]]
                count <- tmp[[2]]
        }
        return(decks)
}

score_winner <- function(decks){
        if((length(decks[[1]])!=0)&(length(decks[[2]])!=0)){
                stop('the game is not over')
        }
        if(length(decks[[1]])==0){
                score <- decks[[2]]*length(decks[[2]]):1
        }else{
                score <- decks[[1]]*length(decks[[1]]):1
        }
        return(sum(score))
}
print(score_winner(play_game(decks)))

# part 2

rec_turn <-function(decks,depth){
        count <<- count + 1
        if(count%%1000==0){
                print(decks)
        }
        tmp_1 <- decks[[1]][1]
        tmp_2 <- decks[[2]][1]
        seen_flag <- FALSE
        if(tmp_1>=length(decks[[1]])|tmp_2>=length(decks[[2]])){
                if(tmp_1>tmp_2){
                        deck_1 <- c(tmp_1,tmp_2)
                        deck_2 <- NULL
                }else{
                        deck_1 <- NULL
                        deck_2 <- c(tmp_2,tmp_1)
                }
                if(length(decks[[1]])>1){
                        deck_1 <- c(decks[[1]][2:length(decks[[1]])],deck_1)
                }
                if(length(decks[[2]])>1){
                        deck_2 <- c(decks[[2]][2:length(decks[[2]])],deck_2)
                }
                return(list(deck_1,deck_2))
        }else{
                seen <- NULL
                sub_decks <- list(decks[[1]][2:(tmp_1+1)],decks[[2]][2:(tmp_2+1)])
                while((length(sub_decks[[1]])!=0)&(length(sub_decks[[2]])!=0)){
                        sub_decks <- rec_turn(sub_decks,depth+1)
                        dk_test <- paste(sub_decks[[1]],0,sub_decks[[2]],collapse='',sep='')
                        if(dk_test %in% seen){
                                seen_flag = TRUE
                                break
                        }else{
                                seen <- c(seen,dk_test)
                        }
                }
                if(seen_flag){
                        deck_1 <- c(tmp_1,tmp_2)
                        deck_2 <- NULL
                }else if(length(sub_decks[[2]])==0){
                        deck_1 <- c(tmp_1,tmp_2)
                        deck_2 <- NULL
                }else{
                        deck_1 <- NULL
                        deck_2 <- c(tmp_2,tmp_1) 
                }
                if(length(decks[[1]])>1){
                        deck_1 <- c(decks[[1]][2:length(decks[[1]])],deck_1)
                }
                if(length(decks[[2]])>1){
                        deck_2 <- c(decks[[2]][2:length(decks[[2]])],deck_2)
                }
                return(list(deck_1,deck_2))
        }
}

play_rec_game <- function(decks){
        count <- 0
        seen <- NULL
        while((length(decks[[1]])!=0)&(length(decks[[2]])!=0)){
                dk_test <- paste(decks[[1]],0,decks[[2]],collapse='',sep='')
                if(dk_test %in% seen){
                        break
                }
                count <<-0
                seen <- c (seen,dk_test)
                decks <- rec_turn(decks,0)
                count <- count + 1
                print(count)
        }
        return(decks)  
}




rec_turn <-function(decks,depth){
        count <<- count + 1
        if(count%%1000==0){
                print(decks)
        }
        tmp_1 <- decks[[1]][1]
        tmp_2 <- decks[[2]][1]
        seen_flag <- FALSE
        if(tmp_1>=length(decks[[1]])|tmp_2>=length(decks[[2]])){
                if(tmp_1>tmp_2){
                        deck_1 <- c(tmp_1,tmp_2)
                        deck_2 <- NULL
                }else{
                        deck_1 <- NULL
                        deck_2 <- c(tmp_2,tmp_1)
                }
                if(length(decks[[1]])>1){
                        deck_1 <- c(decks[[1]][2:length(decks[[1]])],deck_1)
                }
                if(length(decks[[2]])>1){
                        deck_2 <- c(decks[[2]][2:length(decks[[2]])],deck_2)
                }
                return(list(deck_1,deck_2))
        }else{
                seen <- NULL
                sub_decks <- list(decks[[1]][2:(tmp_1+1)],decks[[2]][2:(tmp_2+1)])
                while((length(sub_decks[[1]])!=0)&(length(sub_decks[[2]])!=0)){
                        sub_decks <- rec_turn(sub_decks,depth+1)
                        dk_test <- paste(sub_decks[[1]],0,sub_decks[[2]],collapse='',sep='')
                        if(dk_test %in% seen){
                                seen_flag = TRUE
                                break
                        }else{
                                seen <- c(seen,dk_test)
                        }
                }
                if(seen_flag){
                        deck_1 <- c(tmp_1,tmp_2)
                        deck_2 <- NULL
                }else if(length(sub_decks[[2]])==0){
                        deck_1 <- c(tmp_1,tmp_2)
                        deck_2 <- NULL
                }else{
                        deck_1 <- NULL
                        deck_2 <- c(tmp_2,tmp_1) 
                }
                if(length(decks[[1]])>1){
                        deck_1 <- c(decks[[1]][2:length(decks[[1]])],deck_1)
                }
                if(length(decks[[2]])>1){
                        deck_2 <- c(decks[[2]][2:length(decks[[2]])],deck_2)
                }
                return(list(deck_1,deck_2))
        }
}

play_rec_game <- function(decks,level){
        seen <- NULL
        count <- 0
        while((length(decks[[1]])!=0)&(length(decks[[2]])!=0)){
                dk_test <- paste(c(decks[[1]],0,decks[[2]]),collapse=' ',sep='')
                if(dk_test %in% seen){
                        return(decks)
                }
                seen <- c(seen,dk_test)
                tmp_1 <- decks[[1]][1]
                tmp_2 <- decks[[2]][1]
                if(tmp_1>=length(decks[[1]])|tmp_2>=length(decks[[2]])){
                        if(tmp_1>tmp_2){
                                deck_1 <- c(tmp_1,tmp_2)
                                deck_2 <- NULL
                        }else{
                                deck_1 <- NULL
                                deck_2 <- c(tmp_2,tmp_1)
                        }
                        if(length(decks[[1]])>1){
                                deck_1 <- c(decks[[1]][2:length(decks[[1]])],deck_1)
                        }
                        if(length(decks[[2]])>1){
                                deck_2 <- c(decks[[2]][2:length(decks[[2]])],deck_2)
                        }
                        decks <- list(deck_1,deck_2)
                }else{
                        sub_decks <- list(decks[[1]][2:(tmp_1+1)],decks[[2]][2:(tmp_2+1)])
                        sub_decks <- play_rec_game(sub_decks,level+1)
                        if(length(sub_decks[[1]])==0){
                                deck_1 <- NULL
                                deck_2 <- c(tmp_2,tmp_1)
                        }else{
                                deck_1 <- c(tmp_1,tmp_2)
                                deck_2 <- NULL
                        }
                        if(length(decks[[1]])>1){
                                deck_1 <- c(decks[[1]][2:length(decks[[1]])],deck_1)
                        }
                        if(length(decks[[2]])>1){
                                deck_2 <- c(decks[[2]][2:length(decks[[2]])],deck_2)
                        }
                        decks <- list(deck_1,deck_2)
                }
                if(level==0){
                        count <- count + 1
                        print(count)        
                }
        }
        return(decks)  
}

print(score_winner(play_rec_game(decks,0)))