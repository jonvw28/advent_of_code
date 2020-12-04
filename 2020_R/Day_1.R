setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

find_2020<- function(data){
        N_dat <- length(data)
        for(i in 1:N_dat){
                num_1 <- data[i]
                for(j in i:N_dat){
                        num_2 <- data[j]
                        if((num_1+num_2)==2020){
                                print(paste0('solution is place ',i,' with value ',num_1,' and place ',j,' with value ',num_2,' and product ',num_1*num_2))
                                return(num_1*num_2)
                        }
                }
        }
}

data <- read.csv('day_1_data.txt',col.names = F)
find_2020(data[,1])

find_n<- function(data,n){
        N_dat <- length(data)
        for(i in 1:N_dat){
                num_1 <- data[i]
                for(j in i:N_dat){
                        num_2 <- data[j]
                        if((num_1+num_2)==n){
                                print(paste0('solution is place ',i,' with value ',num_1,' and place ',j,' with value ',num_2,' and product ',num_1*num_2))
                                return(c(i,j,num_1,num_2,num_1*num_2))
                        }
                }
        }
        return(NULL)
}

find_2020_b <- function(data){
        N_dat <- length(data)
        for(i in 1:N_dat){
                num_1 <- data[i]
                trial <- find_n(data[i:N_dat],2020-num_1)
                if(!is.null(trial)){
                        print(paste0('solution is place ',i,' with value ',num_1,' and the previous outputs, giving product ',num_1*trial[5]))
                                return(num_1*trial[5])
                }
        }
}

find_2020_b(data[,1])
