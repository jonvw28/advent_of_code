# manually parsed a lot of stuff rather than using bit/binary functions
# very much quick and dirty and not more robust

setwd("D:/jon/Documents/Cache/advent_of_code/2021_R")

data_parser <- function(connection){
        input <- readLines(connection)
        numCol <- nchar(input[1])
        data <- matrix(nrow = length(input),ncol=numCol,data=-1)
        for(i in 1:length(input)){
                for(j in 1:numCol){
                        data[i,j] <- as.numeric(substr(input[i],j,j))
                }
        }
        return(as.data.frame(data))
}

generate_int <- function(input){
        int <- 0
        pow <- 1
        for(i in length(input):1){
                if(input[i]==1){
                        int <- int + pow
                }
                pow <- pow*2
        }
        return(int)
}

calculate_rates <- function(data){
        numCol <- ncol(data)
        rates <- matrix(ncol=numCol,nrow=2,data=-1)
        for(i in 1:numCol){
                a <- sum(data[,i]==0)
                b <- sum(data[,i]==1)
                if(a>b){
                        rates[,i] <- c(0,1)
                }else{
                        rates[,i] <- c(1,0)
                }
        }
        gamma <- generate_int(rates[1,])
        epsilon <- generate_int(rates[2,])
        return(c(gamma,epsilon))
}

data <- data_parser('day_3_data.txt')
prod(calculate_rates(data))


#part 2
calculate_gas_rates <- function(Oxdata,Codata,col){
        # oxygen
        if(nrow(Oxdata)!=1){
                a <- sum(Oxdata[,col]==0)
                b <- sum(Oxdata[,col]==1)
                if(a>b){
                        key <- 0
                }else{
                        key <- 1
                }
                Oxdata <- Oxdata[Oxdata[,col]==key,]
        }
        # CO2
        if(nrow(Codata)!=1){
                a <- sum(Codata[,col]==0)
                b <- sum(Codata[,col]==1)
                if(b<a){
                        key <- 1
                }else{
                        key <- 0
                }
                Codata <- Codata[Codata[,col]==key,]
        }
        
        if((nrow(Oxdata)==1)&&(nrow(Codata)==1)){
                OxNum <- generate_int(Oxdata)
                CoNum <- generate_int(Codata)
                return(c(OxNum,CoNum))
        }else{
                calculate_gas_rates(Oxdata,Codata,col+1)
        }
}

prod(calculate_gas_rates(data,data,1))
