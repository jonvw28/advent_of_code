# This is very much a quick and dirty lots of loops nesting version. Any other
# day I would try to implement some sort of caching of neighbours for each
# tested location in part 2 so that when you visit a location you don't need to
# move along diagonals and rows you;ve already done, BUTTTT, today I am going to
# volunteer in a hospital so just wanted to get the andwer sorted

setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

parse_file <- function(data){
        map <- matrix(nrow=nrow(data),ncol=nchar(data[1,]),data='')
        for(i in 1:nrow(data)){
                lne <- strsplit(data[i,],'')[[1]]
                map[i,] <- lne
        }
        return(map)
}

return_neighbours <- function(map,row,col){
        if(row==1|row==nrow(map)){
                stop('row index is on an edge, pad your map first')
        }
        if(col==1|col==ncol(map)){
                stop('column index is on an edge, pad your map first')
        }
        neighs <- map[(row-1):(row+1),(col-1):(col+1)]
        neighs[2,2] <- '.'
        emp <- sum(neighs=='L')
        full <- sum(neighs=='#')
        return(c(emp,full))
}

return_view <- function(map,row,col){
        if(row==1|row==nrow(map)){
                stop('row index is on an edge, pad your map first')
        }
        if(col==1|col==ncol(map)){
                stop('column index is on an edge, pad your map first')
        }
        neighs <- map[(row-1):(row+1),(col-1):(col+1)]
        # update to look when needed
        for(i in -1:1){
                for(j in -1:1){
                        if(i==0 & j==0){
                                next
                        }else if(neighs[i+2,j+2]=='.'){
                                iloc <-i+row
                                jloc <-j+col
                                val <- '.'
                                while(iloc>0 & jloc>0 & iloc<nrow(map) & jloc<ncol(map) & val=='.'){
                                        val <- map[iloc,jloc]
                                        iloc <- iloc + i
                                        jloc <- jloc + j
                                }
                                neighs[i+2,j+2] <- val
                        }
                }
        }
        neighs[2,2] <- '.'
        emp <- sum(neighs=='L')
        full <- sum(neighs=='#')
        return(c(emp,full))
}

update_map <- function(map,part,thresh){
        padded <- matrix(nrow=nrow(map)+2,ncol=ncol(map)+2 , data='.')
        padded[2:(nrow(map)+1),2:(ncol(map)+1)]<- map
        change <- 0
        for(i in 1:nrow(map)){
                for(j in 1:ncol(map)){
                        if(padded[i+1,j+1]=='.'){
                                next
                        }
                        if(part==1){
                                neighs <- return_neighbours(padded,i+1,j+1)
                        }else if(part==2){
                                neighs <- return_view(padded,i+1,j+1)
                        }else{
                                stop('Well aren\'t you eager, there are no other parts')
                        }
                        if(padded[i+1,j+1]=='L' & neighs[2]==0){
                                map[i,j] <- '#'
                                change <- change+1
                        }else if(padded[i+1,j+1]=='#' & neighs[2]>=thresh){
                                map[i,j] <- 'L'
                                change <- change+1
                        }else{
                                next
                        }
                }
        }
        return(list(map,change))
}

finalise_map <- function(map,part,thresh){
        count <- -1
        change <- 1
        new_map <- map
        while(change != 0){
                map <- new_map
                if(part==1){
                        new <- update_map(map,part,4)        
                }else if(part==2){
                        new <- update_map(map,part,5)
                }else{
                        stop('Well aren\'t you eager, there are no other parts')
                }
                
                new_map <- new[[1]]
                change <- new[[2]]
                count <- count + 1
        }
        print(paste0('Map stabilised after ',count,' iterations'))
        return(map)
}


data <- read.csv('day_11_data.txt',header=F)
map <- parse_file(data)
print(sum(finalise_map(map,1,4)=='#'))
print(sum(finalise_map(map,2,5)=='#'))