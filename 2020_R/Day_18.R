setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

load_data <- function(filepath){
        data <- readLines(filepath)
        data <- gsub(' ','',data)
        input <- strsplit(data,'')
        return(input)
}

evaluate_expr <- function(expr,plus_first){
        if(length(expr)%%2!=1){
                stop('this isn\'t a valid expression')
        }
        if(plus_first){
                plus <- which(expr=='+')
                while(length(plus)>0){
                        reg <- as.numeric(expr[plus[1]-1])+as.numeric(expr[plus[1]+1])
                        if(plus[1]>2){
                                L_pad <- expr[1:(plus[1]-2)]
                        }else{
                                L_pad <- NULL
                        }
                        if(plus[1]<length(expr)-2){
                                R_pad <- expr[(plus[1]+2):length(expr)]
                        }else{
                                R_pad <- NULL
                        }
                        expr <- c(L_pad,reg,R_pad)
                        plus <- which(expr=='+')
                }
                reg <- as.numeric(expr[1])
                if(length(expr)>1){
                        for(i in seq(from=3,to=length(expr),by=2)){
                                if(expr[i-1]=='*'){
                                        reg <- reg*as.numeric(expr[i])
                                }else if(expr[i-1]=='+'){
                                        reg <- reg+as.numeric(expr[i])
                                }else{
                                        stop('You can only use + and * operators')
                                }
                        }
                }
        }else{
                reg <- as.numeric(expr[1])
                if(length(expr)>1){
                        for(i in seq(from=3,to=length(expr),by=2)){
                                if(expr[i-1]=='*'){
                                        reg <- reg*as.numeric(expr[i])
                                }else if(expr[i-1]=='+'){
                                        reg <- reg+as.numeric(expr[i])
                                }else{
                                        stop('You can only use + and * operators')
                                }
                        }
                }
        }
        return(reg)
}

expand_brackets <- function(line,plus_first){
        L_bracks <- which(line=='(')
        if(length(L_bracks)==0){
                return(line)
        }
        if(length(L_bracks)!=sum(line==')')){
                stop('you have an open ended bracket')
        }
        for(L_b in length(L_bracks):1){
                left <- L_bracks[L_b]
                R_bracks <- which(line==')')
                right <- min(R_bracks[R_bracks>left])
                val <- evaluate_expr(line[(left+1):(right-1)],plus_first)
                if(left>1){
                        L_pad <- line[1:(left-1)]
                }else{
                        L_pad <- NULL
                }
                if(right<length(line)){
                        R_pad <- line[(right+1):length(line)]
                }else{
                        R_pad <- NULL
                }  
                line <- c(L_pad,val,R_pad)
        }
        return(line)
}

compute_values <- function(input,plus_first){
        res <- numeric(length(input))
        for(i in 1:length(input)){
                expr <- expand_brackets(input[[i]],plus_first)
                res[i] <- evaluate_expr(expr,plus_first)
        }
        return(res)
}



format(sum(compute_values(load_data('day_18_data.txt'),FALSE)),digits=20)
format(sum(compute_values(load_data('day_18_data.txt'),TRUE)),digits=20)
