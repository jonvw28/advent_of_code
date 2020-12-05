setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

string_to_binary <-function(string){
        if(nchar(string)!=10){
                stop('this isn\'t a boarding pass - it\'s the wrong length')
        }
        row <- 0
        for(i in 6:0){
                char <- substring(string,first=7-i,last=7-i)
                if(char=='B'){
                        row <- row + 2^i
                }else if(char!='F'){
                        stop('the row specifier is corrupt')
                }
        }
        seat <- 0
        for(j in 2:0){
                char <- substring(string,first=10-j,last=10-j)
                if(char=='R'){
                        seat <- seat + 2^j
                }else if(char!='L'){
                        stop('the seat specifier is corrupt')
                }
        }
        return(c(row,seat,8*row+seat))
}


data <- read.csv('day_5_data.txt',header=F)

max_id <- -1

for(i in 1:nrow(data)){
        seat_id <- string_to_binary(data[i,])
        if(seat_id[3]> max_id){
                max_id <- seat_id[3]
        }
}
print(max_id)

### part 2

get_seat_list <- function(data){
        seats <- NULL
        for(i in 1:nrow(data)){
                ths_id <- string_to_binary(data[i,])
                seats <- c(seats,ths_id[3])
        }
        return(sort(seats))
}

find_seat <- function(seat_list){
        loc <- which(diff(seat_list)==2)
        return(seat_list[loc]+1)
}
print(find_seat(get_seat_list(data)))

