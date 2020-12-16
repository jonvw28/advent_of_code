setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

parse_step <- function(inst){
        act <- substring(inst,first=1,last=1)
        num <- as.numeric(substring(inst,first=2,last=nchar(inst)))
        return(list(act,num))
}


apply_step <- function(state,inst){
        step <- parse_step(inst)
        N <- state[1]
        E <- state[2]
        dir <- state[3]
        if(step[[1]][1]=='L'){
                dir <- (dir- step[[2]][1])%%360
        }else if(step[[1]][1]=='R'){
                dir <- (dir + step[[2]][1])%%360
        }else if(step[[1]][1]=='N'){
                N <- N + step[[2]][1]
        }else if(step[[1]][1]=='E'){
                E <- E + step[[2]][1]
        }else if(step[[1]][1]=='S'){
                N <- N - step[[2]][1]
        }else if(step[[1]][1]=='W'){
                E <- E - step[[2]][1]
        }else if(step[[1]][1]=='F'){
                if(dir==0){
                        N <- N + step[[2]][1]
                }else if(dir==90){
                        E <- E + step[[2]][1]
                }else if(dir==180){
                        N <- N - step[[2]][1]
                }else if(dir==270){
                        E <- E - step[[2]][1]
                }else{
                        stop('your direction is not cardinal')
                }
        }else{
                stop('this isn\'t a valid instruction')
        }
        return(c(N,E,dir))
}

run_ins_set <- function(insts){
        state <- c(0,0,90)
        for(i in 1:length(insts)){
                state <- apply_step(state,insts[i])
        }
        return(state)
}

data <- read.csv('day_12_data.txt',header=F)[,1]
final <- run_ins_set(data)
print(abs(final[1])+abs(final[2]))

# Part 2
apply_step_wp <- function(ship,wp,inst){
        shipN <- ship[1]
        shipE <- ship[2]
        wpN <- wp[1]
        wpE <- wp[2]
        step <- parse_step(inst)
        com <- step[[1]]
        val <- step[[2]]
        if(com=='N'){
                wpN <- wpN + val
        }else if(com=='E'){
                wpE <- wpE + val
        }else if(com=='S'){
                wpN <- wpN - val
        }else if(com=='W'){
                wpE <- wpE - val
        }else if(com=='L'){
                val <- val%%360
                if(val==90){
                        cache <- wpN
                        wpN <- wpE
                        wpE <- -cache
                }else if(val==180){
                        wpN <- -wpN
                        wpE <- -wpE
                }else if(val==270){
                        cache <- wpN
                        wpN <- -wpE
                        wpE <- cache
                }else if(val==0){
                }else{
                        stop('error: your angle is not cardinal')
                }
        }else if(com=='R'){
                val <- val%%360
                if(val==90){
                        cache <- wpN
                        wpN <- -wpE
                        wpE <- cache
                        cache <- wpN
                }else if(val==180){
                        wpN <- -wpN
                        wpE <- -wpE
                }else if(val==270){
                        cache <- wpN
                        wpN <- wpE
                        wpE <- -cache
                }else if(val==0){
                }else{
                        stop('error: your angle is not cardinal')
                }
        }else if(com=='F'){
                shipN <- shipN + val*wpN
                shipE <- shipE + val*wpE
        }else{
                stop('this isn\'t a valid instruction')
        }
        return(c(shipN,shipE,wpN,wpE))
}

run_wp_set <- function(insts){
        state <- c(0,0,1,10)
        for(i in 1:length(insts)){
                state <- apply_step_wp(state[1:2],state[3:4],insts[i])
        }
        return(state)
}

run_wp_set(data)
final2 <- run_wp_set(data)
print(abs(final2[1])+abs(final2[2]))
