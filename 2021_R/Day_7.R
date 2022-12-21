# Below was very lazy copy paste for part 2 rather than adding an
# option to the original functions, but not all that different


setwd("D:/jon/Documents/Cache/advent_of_code/2021_R")

data_parser <- function(connection){
        data <- readLines(connection)
        data <- as.numeric(strsplit(data,",")[[1]])
        data <- factor(data,levels=0:max(data))
        return(as.numeric(table(data)))
}

build_fuel <- function(len,spot){
        if(spot==1){
                fuelVec <- 0:(len-1)
        }else if(spot==len){
                fuelVec <- (len-1):0
        }else{
                fuelVec <- numeric(len)
                fuelVec[1:(spot-1)] <- (spot-1):1
                fuelVec[(spot+1):len] <- 1:(len-spot)
        }
        return(fuelVec)
}

find_min <- function(data){
        len <- length(data)
        minI <- 0
        minV <- sum(data*build_fuel(len,1))
        for(i in 2:len){
                newV <- sum(data*build_fuel(len,i))
                if(newV < minV){
                        minV <- newV
                        minI <- i-1
                }
        }
        return(c(minI,minV))
}

data <- data_parser("day_7_data.txt")
find_min(data)


### Part 2
build_fuel_2 <- function(len,spot){
        if(spot==1){
                fuelVec <- (0:(len-1)*1:(len))/2
        }else if(spot==len){
                fuelVec <- ((len-1):0*len:1)/2
        }else{
                fuelVec <- numeric(len)
                fuelVec[1:(spot-1)] <- ((spot-1):1*spot:2)/2
                fuelVec[(spot+1):len] <- (1:(len-spot)*2:(len-spot+1))/2
        }
        return(fuelVec)
}

find_min_2 <- function(data){
        len <- length(data)
        minI <- 0
        minV <- sum(data*build_fuel_2(len,1))
        for(i in 2:len){
                newV <- sum(data*build_fuel_2(len,i))
                if(newV < minV){
                        minV <- newV
                        minI <- i-1
                }
        }
        return(c(minI,minV))
}

find_min_2(data)
