setwd("D:/jon/Documents/Cache/advent_of_code/2021_R")

data_parser <- function(connection){
        data <- readLines(connection)
        instruction <- character()
        move <- numeric()
        for(i in 1:length(data)){
            line <- strsplit(data[i]," ")
            instruction <- c(instruction,line[[1]][1])
            move <- as.numeric(c(move,line[[1]][2]))
        }
        return(data.frame(instruction,move))
}

action_instructions <- function(steps){
        loc <- c(0,0)
        for(i in 1:nrow(steps)){
                action <- steps[i,1]
                if(action=="forward"){
                        loc[1] <- loc[1] + steps[i,2]
                }else if(action=="up"){
                        loc[2] <- loc[2] - steps[i,2]
                }else if(action=="down"){
                        loc[2] <- loc[2] + steps[i,2]
                }else{
                        stop("invalid instruction")
                }
        }
        return(loc)
}


data <- data_parser('day_2_data.txt')
final <- action_instructions(data)
prod(final)

# part 2

action_instructions_pt2 <- function(steps){
        loc <- c(0,0,0)
        for(i in 1:nrow(steps)){
                action <- steps[i,1]
                if(action=="forward"){
                        loc[2] <- loc[2] + loc[3]*steps[i,2]
                        loc[1] <- loc[1] + steps[i,2]
                }else if(action=="up"){
                        loc[3] <- loc[3] - steps[i,2]
                }else if(action=="down"){
                        loc[3] <- loc[3] + steps[i,2]
                }else{
                        stop("invalid instruction")
                }
        }
        return(loc)
}
final_pt2 <- action_instructions_pt2(data)
prod(final_pt2[1:2])