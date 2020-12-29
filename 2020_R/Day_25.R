setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

get_pub <- function(filepath){
        data <- read.csv(filepath,header=FALSE)
        return(data[,1])
}
vals <- get_pub('day_25_data.txt')

apply_step <- function(val,sub,div){
        return((val*sub)%%div)
}

find_key <- function(trgt){
        val <- 1
        cnt <- 0
        while(val != trgt & cnt < 20201228){
                val <- apply_step(val,7,20201227)
                cnt <- cnt + 1
        }
        if(cnt>20201227){
                stop('we failed to find the key')
        }
        return(cnt)
}

get_enc <- function(key,sub){
        val <- 1
        for(i in 1:key){
                val <- apply_step(val,sub,20201227)
        }
        return(val)
}

key1 <- find_key(vals[1])
key2 <- find_key(vals[2])
print(get_enc(key2,vals[1]))
print(get_enc(key1,vals[2]))
