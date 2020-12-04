setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

data <- read.csv('day_3_data.txt',sep = '',header=F)
# I am starting to realise just how much using R for all of this year makes handling strings a pain
# Wanted to use this as my choice though to show I could
map <- matrix(nrow=nrow(data),ncol=nchar(data[1,1]))
for(i in 1:nrow(data)){
        lne <- strsplit(data[i,],'')[[1]]
        map[i,] <- 1*(lne=='#')
}

take_step <- function(rin,cin,rstep,cstep,map){
        rloc <- rin + rstep
        cloc <- cin + cstep
        if(rloc > nrow(map)){
                stop('You\' tried to go beyond the end of the map, careful now')
        }
        if(cloc%%ncol(map)==0){
                return(c(map[rloc,ncol(map)],rloc,cloc))
        }else{
                return(c(map[rloc,cloc%%ncol(map)],rloc,cloc))
        }
        
}

rloc <- 1
cloc <- 1
tally <- 0
for(i in 2:nrow(map)){
        output <- take_step(rloc,cloc,1,3,map)
        tally <- tally + output[1]
        rloc<- output[2]
        cloc<- output[3]
}
print(tally)

##### part 2

get_trees <- function(rstep,cstep,map){
        rloc <- 1
        cloc <- 1
        tally <- 0
        for(i in 1:((nrow(map)-1)/rstep)){
                output <- take_step(rloc,cloc,rstep,cstep,map)
                tally <- tally + output[1]
                rloc<- output[2]
                cloc<- output[3]
        }
        return(tally)
}
print(get_trees(1,1,map)*get_trees(1,3,map)*get_trees(1,5,map)*get_trees(1,7,map)*get_trees(2,1,map))