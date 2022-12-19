setwd("D:/jon/Documents/Cache/advent_of_code/2021_R")

tidy_dual_split <- function(data,splitter){
        newData <- strsplit(data, splitter)
        list1 <- unlist(newData)[2*(1:length(data))-1]
        list2 <- unlist(newData)[2*(1:length(data))]
        return(list(list1,list2))
}


parse_data <- function(connection){
        data <- readLines(connection)
        splitData <- tidy_dual_split(data," -> ")
        fromCoord <- tidy_dual_split(splitData[[1]],",")
        toCoord <- tidy_dual_split(splitData[[2]],",")
        x1 <- as.numeric(fromCoord[[1]])
        y1 <- as.numeric(fromCoord[[2]])
        x2 <- as.numeric(toCoord[[1]])
        y2 <- as.numeric(toCoord[[2]])
        return(list(x1,y1,x2,y2))
}

build_map <- function(data,diag){
        x1 <- data[[1]]
        y1 <- data[[2]]
        x2 <- data[[3]]
        y2 <- data[[4]]
        mapMat <- matrix(nrow=max(y1,y2)+1,ncol=max(x1,x2)+1,data=0)
        for(i in 1:length(x1)){
                if(x1[i]==x2[i]){
                        x <- x1[i]
                        y <- y1[i]:y2[i]
                        mapMat[y+1,x+1] <- mapMat[y+1,x+1]+1
                }else if(y1[i]==y2[i]){
                        y <- y1[i]
                        x <- x1[i]:x2[i]
                        mapMat[y+1,x+1] <- mapMat[y+1,x+1]+1
                }else if(diag==TRUE){
                        y <- y1[i]:y2[i]
                        x <- x1[i]:x2[i]
                        coord <- cbind(y+1,x+1)
                        mapMat[coord] <- mapMat[coord]+1
                }     
        }
        return(sum(mapMat>1))
}

data <- parse_data("day_5_data.txt")
build_map(data,FALSE)
build_map(data,TRUE)