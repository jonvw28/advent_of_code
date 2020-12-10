setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

generate_init_lookup <- function(data,n){
        lookup <- matrix(nrow=n,ncol=n,data=0)
        nums<-data[1:n]
        for(i in 1:n){
                lookup[i,-i] <- nums[-i] + nums[i]
        }
        return(list(data[1:n],lookup))
}

update_lookup <- function(lookup,n,value){
        loc <- n%%ncol(lookup[[2]])
        if(loc==0){
                loc=ncol(lookup[[2]])
        }
        lookup[[2]][loc,-loc] <- lookup[[1]][-loc] + value
        lookup[[2]][-loc,loc] <- lookup[[1]][-loc] + value
        lookup[[1]][loc]<-value
        return(lookup)
}

find_exception<-function(data,n){
        lookup <- generate_init_lookup(data,n)
        loc <- n+1
        value <- data[n+1]
        while(loc < length(data) & value %in% lookup[[2]]){
                lookup <- update_lookup(lookup,loc,value)
                loc <- loc + 1
                value <- data[loc]
        }
        if(loc==length(data) & value %in% lookup[[2]]){
                warning('All numbers are valid, returning final vlaue of data')
        }
        print(paste0('Exception value occurs in position ',loc))
        return(value)
}

data <- as.numeric(read.csv('day_9_data.txt',header=F)[,1])
print(find_exception(data,25))

# part 2

init_contig <- function(data,value){
        sum <- c(0,0)
        loc <- 0
        while(sum[2]<=value & loc<length(data)){
                loc <- loc + 1
                sum[1]<-sum[2]
                sum[2]<-sum[2]+data[loc]
                
        }
        if(loc==length(data) & sum[2]<value){
                warning('you can\'t get a value this big even with all the data')
                return(c(1,loc,sum[2]))
        }
        
        return(c(1,loc-1,sum[1]))
}

update_contig <- function(contig,data,value){
        init_loc <- contig[1]
        final_loc <- contig[2]+1
        score <- contig[3] + data[final_loc]
        while(score>value & init_loc < final_loc){
                score <- score - data[init_loc]
                init_loc <- init_loc+1
        }
        if(init_loc==final_loc){
                if(sum(data[init_loc:length(data)]<=value)==0){
                        warning('Seems you have a value for which there is no contig, is part a right?')
                        return(NULL)
                }else{
                        init_loc <- min(which(data[init_loc:length(data)]<=value))+init_loc-1
                        final_loc <- init_loc
                        score<- data[init_loc]
                }
        }
        sum <- c(score,score)
        while(sum[2]<=value & final_loc < length(data)){
                final_loc <- final_loc + 1
                sum[1]<-sum[2]
                sum[2]<-sum[2]+data[final_loc]
        }
        if(final_loc == length(data) & sum[2]!=value){
                warning('Ran out of data and didn\'t find value, returning contig in state as exhausted')
        }
        return(c(init_loc,final_loc-1,sum[1]))
}

find_contig <- function(data,value){
        i=1
        contig <- init_contig(data,value)
        while(contig[3]!=value & contig[2]< length(data)){
                contig<-update_contig(contig,data,value)
                i<- i +1
                if(is.null(contig)){
                        stop('Seems your value has no contig')
                }
        }
        return(contig)
}


contig_to_weak <- function(contig,data){
        return(min(data[contig[1]:contig[2]])+max(data[contig[1]:contig[2]]))
}

print(contig_to_weak(find_contig(data,find_exception(data,25)),data))