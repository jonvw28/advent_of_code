setwd("D:/jon/Documents/Cache/advent_of_code/2021_R")


data_parser <- function(connection){
        lines <- readLines(connection)
        vals <- strsplit(lines,'')
        return(vals)
}

check_line <- function(line,part){
        front <- c("(","{","[","<")
        stack <- c()
        while(length(line)>0){
                brack <- line[1]
                if(brack %in% front){
                        stack <- c(brack,stack)
                }else if(length(stack)==0){
                        stop("Appears to be an illegal closed statement")
                }else if(brack == ")" && stack[1] == "("){
                        stack <- stack[-1]
                }else if(brack == "]" && stack[1] == "["){
                        stack <- stack[-1]
                }else if(brack == "}" && stack[1] == "{"){
                        stack <- stack[-1]
                }else if(brack == ">" && stack[1] == "<"){
                        stack <- stack[-1]
                }else{
                        return(brack)
                }
                line <- line[-1]
        }
        if(part==1){
                return(0)  
        }else if(part==2){
                return(stack)
        }else{
                stop("Someone forgot how AoC works")
        }
}

score_errors <- function(data){
        score <- 0
        for(i in 1:length(data)){
                res <- check_line(data[[i]],1)
                if(res == ")"){
                        score <- score + 3
                }else if(res == "]"){
                        score <- score + 57
                }else if(res == "}"){
                        score <- score + 1197
                }else if(res == ">"){
                        score <- score + 25137
                }
        }
        return(score)
}

data <- data_parser("day_10_data.txt")
score_errors(data)

finish_line <- function(stack){
        score <- 0 
        while(length(stack)>0){
                score <- score*5
                res <- stack[1]
                if(res == "("){
                        score <- score + 1
                }else if(res == "["){
                        score <- score + 2
                }else if(res == "{"){
                        score <- score + 3
                }else if(res == "<"){
                        score <- score + 4
                }
                stack <- stack[-1]
        }
        return(score)
}

score_completion <- function(data){
        res <- c()
        for(i in 1:length(data)){
                toDo <- check_line(data[[i]],2)
                if((length(toDo) == 1) && (toDo %in% c(")","]","}",">"))){
                        next
                }else{
                     res <- c(res,finish_line(toDo))  
                }
        }
        return(median(res))
}
score_completion(data)