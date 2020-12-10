setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

get_diffs<- function(data,n){
        chain <- sort(data)
        chain <- c(0,chain,max(chain)+n)
        jumps <- diff(chain)
        if(min(jumps)<1){
                stop('seems you\'ve got two adapters with the same rating, try removing one')
        }
        if(max(jumps)>n){
                stop('You\'re trying to complete a chain with maximum gap too big for the adapters you have')
        }
        gaps <- 1:n
        tallies <- numeric(length=n)
        for(delt in 1:n){
                tallies[delt]<-sum(jumps==delt)
        }
        return(data.frame(gaps=gaps,tallies=tallies))
}


data <- as.numeric(read.csv('day_10_data.txt',stringsAsFactors = F,header=F)[,1])
tally <- get_diffs(data,3)
print(tally[1,2]*tally[3,2])

## part 2
isolate_parts<- function(data,n){ # break into isolated parts which we can enumerate independently and multiply
        chain <- sort(data)
        chain <- c(0,chain,max(chain)+n)
        jumps <- diff(chain)
        breaks <- which(jumps==n)+1
        return(data.frame(loc=c(1,breaks),adapt=c(0,chain[breaks])))
        
}

enumerate_paths<- function(adapts,from,to,n){
        if(from==to){ # just in case
                stop('you can\'t just jump from an adapter to itself')
        }
        if(length(adapts)==0){ # ie check the jump is allowed
                if((to-from)<=n){
                        return(1)
                }else{
                        stop('that\'s an impossible jump with no adapters')
                }
        }
        adapts <- sort(adapts) # list the adapters we have to work with
        if(min(adapts)<=from | max(adapts)>=to){ # only use the adapters within the range (no end point)
                stop('you\'ve included impossible adapters')
        }
        cands <- which(adapts<=from+n)
        if(length(cands)==0){
                stop('you\'ve not included an adapter needed to make the jump in this block')
        }
        tot <- 0
        if((to-from)<=n){
                tot <- tot+1
        }
        for(cand in cands){
                if(cand==length(adapts)){
                        tot <- tot + enumerate_paths(numeric(0),adapts[cand],to,n)
                }else{
                        tot <- tot  + enumerate_paths(adapts[(cand+1):length(adapts)],adapts[cand],to,n)
                }
                
        }
        return(tot)
}

count_all_paths <- function(data,n){
        adapts <- sort(data)
        adapts <- c(0,adapts,max(adapts)+n)
        parts <- isolate_parts(data,n)
        routes <- 1
        for(i in 2:nrow(parts)){
                if((parts[i,1]-parts[i-1,1])==1){
                        next
                }else{
                        routes <- routes*enumerate_paths(adapts[(parts[i-1,1]+1):(parts[i,1]-1)],parts[i-1,2],parts[i,2],n)
                }
        }
        return(routes)
}
print(count_all_paths(data,3),digits=22)
