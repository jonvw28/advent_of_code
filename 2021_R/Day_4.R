setwd("D:/jon/Documents/Cache/advent_of_code/2021_R")


parse_card <- function(lines){
        card <- matrix(nrow=5,ncol=5,data=-1)
        for(i in 1:5){
                vals <- strsplit(lines[i],' ')[[1]]
                vals <- vals[vals!=""] # clear out double space gaps
                vals <- as.numeric(vals)
                card[i,] <- vals
        }
        return(card)
}

parse_data <- function(connection){
        data <- readLines(connection)
        calls <- as.numeric(strsplit(data[1],',')[[1]])
        num_card <- (length(data)-1)/6
        cards_list <- list()
        for(i in 1:num_card){
                cards_list[[i]] <- parse_card(data[(6*(i-1)+3):(6*(i-1)+7)])
        } 
        return(list(calls,cards_list))
}

check_card <- function(card){
        tests <- c(rowSums(card),colSums(card))
        if(sum(tests==0)==0){
                return(FALSE)
        }else{
                return(TRUE)
        }
}

run_game <- function(input){
        calls <- input[[1]]
        cards <- input[[2]]
        num_cards <- length(cards)
        ticker <- 0
        wins <- logical(length=num_cards)
        while(ticker<length(calls)){
                val <- calls[ticker+1]
                for(i in 1:num_cards){
                        cards[[i]][cards[[i]]==val]<-0
                        wins[i] <- check_card(cards[[i]])
                }
                if(sum(wins)!=0){
                        break
                }
                ticker <- ticker + 1
        }
        winner <- cards[[which(wins)]]
        score <- val*sum(winner)
        return(score)
}

data <- parse_data("day_4_data.txt")
run_game(data)

# part 2

# Not robust at all - just know input will behave well enough in AoC
find_loser <- function(input){
        calls <- input[[1]]
        cards <- input[[2]]
        num_cards <- length(cards)
        ticker <- 0
        left <- num_cards
        while(ticker<length(calls)){
                wins <- logical(length=left)
                val <- calls[ticker+1]
                for(i in left:1){
                        cards[[i]][cards[[i]]==val]<-0
                        if(check_card(cards[[i]])){
                                cards[[i]]<- NULL
                        }
                }
                left <- length(cards)
                if(left==1){
                        break
                }
                ticker <- ticker + 1
        }
        return(run_game(list(calls,cards)))
}
find_loser(data)