setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

parse_input <- function(filepath){
        arrival <- as.numeric(readLines(filepath,1))
        routes <- read.csv(filepath,skip=1,header=F)
        rems <- which(routes[1,]!='x')-1
        routes <- routes[1,rems+1]
        routes <- as.numeric(routes[1,])
        return(list(arrival,routes,rems))
}


find_earliest <- function(init,routes){
        best <- routes[1]
        bwait <- routes[1] - init%%routes[1]
        if(length(routes>1)){
                for(i in 2:length(routes)){
                        wait <- routes[i]-init%%routes[i]
                        if(wait<bwait){
                                best <- routes[i]
                                bwait <- wait
                        }
                }
        }
        return(c(best,bwait))
}

input <- parse_input('day_13_data.txt')
part1 <- find_earliest(input[[1]][1],input[[2]])
print(part1[1]*part1[2])

# part 2

# We really ought to check that the routes are pairwise coprime (ie we can
# use this quickly coded version of the way of applying the chinese remainder
# theorem.
#
find_align <- function(routes,rems){
        idx <- sort(routes,decreasing = T, index.return=T)
        routes <- routes[idx$ix]
        rems <- rems[idx$ix]
        satis <- routes[1]-rems[1]
        if(length(routes)>1){
                for(i in 2:length(routes)){
                        ths_cnd <- rems[i]%%routes[i]
                        rem <- routes[i]-satis%%routes[i]
                        if(rem==ths_cnd){
                                next
                        }
                        count <- 0
                        cand <- satis
                        while(rem!=ths_cnd & count <= routes[i]+1){
                                cand <- cand + prod(routes[1:(i-1)])
                                rem <- (routes[i]-cand%%routes[i])%%routes[i]
                                count <- count + 1
                        }
                        if(rem!=ths_cnd){
                                stop('something has gone wrong, probbaly with this quick implementation')
                        }
                        satis <- cand
                }
        }
        return(satis)
}
format(find_align(input[[2]],input[[3]]),digits=20)
