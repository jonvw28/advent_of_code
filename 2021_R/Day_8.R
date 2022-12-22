setwd("D:/jon/Documents/Cache/advent_of_code/2021_R")

tidy_dual_split <- function(data,splitter){
        newData <- strsplit(data, splitter)
        list1 <- unlist(newData)[2*(1:length(data))-1]
        list2 <- unlist(newData)[2*(1:length(data))]
        return(list(list1,list2))
}

data_parser <- function(connection){
        data <- readLines(connection)
        dispDigit <- tidy_dual_split(data," \\| ")
        return(list(strsplit(dispDigit[[1]]," "),strsplit(dispDigit[[2]]," ")))
}

part_1 <- function(data){
        segs <- nchar(unlist(data))
        count <- 0
        for(num in c(2,3,4,7)){
                count <- count + sum(segs==num)
        }
        return(count)
}

data <- data_parser("day_8_data.txt")
part_1(data[[2]])


## part 2

# Below I explain the mapping generation, but in hindsight
# I may have instead built the digit parser and then built
# a subset of the logic below where num of char isn't the
# sole determinant

## Logic I used (not necessarily optimal)
# the extra character in len 3 vs len 2 = A
# from len 6 the three characters appearing twice are cde
# compare to len 2 to get C and also F
# from len 4 now have b and d
# compare to len 6 subset above to get D and thus E and B
# Finally now have G

get_mapping <- function(data){
        map <- character(7)
        segs <- nchar(data)
        # get a
        segs2 <- unlist(strsplit(data[segs==2],""))
        segs3 <- unlist(strsplit(data[segs==3],""))
        map[1] <- segs3[!(segs3 %in% segs2)]
        # get cde
        segs6 <- unlist(strsplit(data[segs==6],""))
        # get c f
        cde <- names(table(segs6)[table(segs6)==2])
        map[3] <- segs2[(segs2 %in% cde)]
        map[6] <- segs2[!(segs2 %in% cde)]
        # get bd
        segs4 <- unlist(strsplit(data[segs==4],""))
        bd <- segs4[!(segs4 %in% segs2)]
        # get b d e
        map[2] <- bd[!(bd %in% cde)]
        map[4] <- bd[(bd %in% cde)]
        map[5] <- cde[!(cde %in% c(map[3],map[4]))]
        # get g
        map[7] <- letters[1:7][!(letters[1:7] %in% map)]
        return(map)
}

get_digit <- function(digit,map){
        if(nchar(digit)==2){
                return(1)
        }else if(nchar(digit)==3){
                return(7)
        }else if(nchar(digit)==4){
                return(4)
        }else if(nchar(digit)==7){
                return(8)
        }else if(nchar(digit)==5){
                segs <- unlist(strsplit(digit,""))
                segs <- map %in% segs
                if(segs[3]&segs[5]){
                        return(2)
                }else if (segs[3]&segs[6]){
                        return(3)
                }else{
                        return(5)
                }
        }else{
                segs <- unlist(strsplit(digit,""))
                segs <- map %in% segs
                if(!segs[4]){
                        return(0)
                }else if(!segs[3]){
                        return(6)
                }else{
                        return(9)
                }
        }
        
}

get_number <- function(output,map){
        num <- 0
        for(i in 1:4){
               dig <- get_digit(output[i],map)
               num <- num + dig*10^(4-i)
        }
        return(num)
}


part_2 <- function(data){
        input <- data[[1]]
        output <- data[[2]]
        tot <- 0
        for(i in 1:length(input)){
                map <- get_mapping(input[[i]])
                num <- get_number(output[[i]],map)
                tot <- tot + num
        }
        return(tot)
}

part_2(data)