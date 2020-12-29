setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

load_data <- function(fielpath){
        line <- readLines(fielpath)
        data <- strsplit(line,'')[[1]]
        return(as.numeric(data))
}

input <- load_data('day_23_data.txt')

get_dest <- function(value,pk_cups,length){
        dest <- value-1
        if(dest==0){
                dest <- length
        }
        while(dest %in% pk_cups){
                dest <- dest-1
                if(dest==0){
                        dest <- length
                }       
        }
        return(dest)
}

take_step <- function(cups,cur){
        cp_ln <- length(cups)
        value <- cups[cur]
        pickup <- cur + 1:3
        pickup[pickup>cp_ln]<-pickup[pickup>cp_ln]-cp_ln
        pk_cups <- cups[pickup]
        cups <- cups[-pickup]
        dest <- which(cups==get_dest(value,pk_cups,cp_ln))
        if(dest<(cp_ln-3)){
               cups[(dest+4):cp_ln]<-cups[(dest+1):(cp_ln-3)] 
        }
        cups[(dest+1):(dest+3)] <- pk_cups
        new_loc <- which(cups==value)+1
        if(new_loc>cp_ln){
                new_loc <- new_loc-cp_ln
        }
        return(list(cups,new_loc))
}

run_game <- function(input,n_step){
        res <- take_step(input,1)
        for(i in 2:n_step){
                res <- take_step(res[[1]],res[[2]])
                if(i%%10000==0){print(i)}
        }
        return(res)
}

final_state <- function(res){
        nums <- res[[1]]
        cp_ln <- length(nums)
        loc_1 <- which(nums == 1)
        out <- numeric(8)
        if(loc_1 < cp_ln){
                out[1:(cp_ln-loc_1)]<-nums[(loc_1+1):cp_ln]
        }
        if(loc_1>1){
                out[(cp_ln-loc_1+1):(cp_ln-1)] <- nums[1:(loc_1-1)]
        }
        return(paste(out,sep='',collapse=''))
}

print(final_state(run_game(input,100)))


# for part 2 we need a much more efficient way to store an access our data as
# the hashing is awful

parse_list <- function(input,length){
        out <- numeric(length)
        out[(length(input)):(length-1)] <- (length(input)+1):length
        out[length] <- input[1]
        for(i in 2:length(input)){
                out[input[i-1]]<-input[i]
        }
        if(length>length(input)){
                out[input[length(input)]]<-length(input)+1
        }else{
                out[input[length(input)]]<-input[1]
        }
        return(out)
}

take_turn_eff <- function(loc){
        pk_cp <- numeric(3)
        pk_cp[1] <- num_list[loc]
        for(i in 2:3){
                pk_cp[i]<-num_list[pk_cp[i-1]]
        }
        num_list[loc]<<-num_list[pk_cp[3]]
        ins <- get_dest(loc,pk_cp,length(num_list))
        num_list[pk_cp[3]]<<-num_list[ins]
        num_list[ins] <<- pk_cp[1]
        new_loc <- num_list[loc]
        return(new_loc)
}

run_game_eff <- function(input,leng,n_step){
        num_list <<- parse_list(input,leng)
        res <- take_turn_eff(input[1])
        for(i in 2:n_step){
                res <- take_turn_eff(res)
                if(i%%1000000==0){print(i)}
        }
        cup1 <- num_list[1]
        cup2 <- num_list[cup1]
        return(cup1*cup2)
}

print(format(run_game_eff(input,10^6,10^7),20))
