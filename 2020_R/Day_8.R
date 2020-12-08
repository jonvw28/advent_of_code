setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

data <- readLines('day_8_data.txt')

txt_to_ticker <- function(data){
        inst <- character(length(data))
        value <- numeric(length(data))
        parts <- strsplit(data,' ')
        for(i in 1:length(data)){
                inst[i] <- parts[[i]][1]
                value[i] <- as.numeric(parts[[i]][2])
        }
        ticker <- data.frame(inst = inst,value = value)
        return(ticker)
}

run_until_issue <- function(ticker,return_bad_acc,print_flag){
        called <- logical(length = nrow(ticker))
        flag <- TRUE
        loc <- 1
        acc <- 0
        while(flag){
                if(loc > nrow(ticker)){
                        if(print_flag){
                                print(paste0('the instruction at line ',loc,' runs off the tape'))
                        }
                                return(acc)
                }else if(called[loc]==TRUE){
                        if(print_flag){
                                print(paste0('the infinite loop starts at line ',loc))
                        }
                        if(return_bad_acc){
                                return(acc)
                        }else{
                                return(NULL)
                        }
                        
                }
                called[loc]=TRUE
                inst <- ticker[loc,1]
                if(inst=='nop'){
                        loc <- loc + 1
                        next
                }else if(inst=='acc'){
                        acc <- acc + ticker[loc,2]
                        loc <- loc + 1
                        next
                }else if(inst=='jmp'){
                        loc <- loc + ticker[loc,2]
                }else{
                        stop(paste0('the instruction on line ',loc,' is invalid'))
                }
        }
}

ticker <- txt_to_ticker(data)
print(run_until_issue(ticker,TRUE,TRUE))

####### part 2
find_fix <- function(ticker){
        for(loc in 1:nrow(ticker)){
                inst <- ticker[loc,1]
                if(inst=='nop'){
                        tmp_ticker <- ticker
                        tmp_ticker[loc,1] <- 'jmp'
                }else if(inst=='jmp'){
                        tmp_ticker <- ticker
                        tmp_ticker[loc,1] <- 'nop'
                }else if(inst=='acc'){
                        tmp_ticker <- ticker
                }else{
                        stop(paste0('the instruction on line ',loc,' is invalid'))
                }
                value <- run_until_issue(tmp_ticker,FALSE,FALSE)
                if(!is.null(value)){
                        print(paste0('the issue was on line ',loc))
                        return(value)
                }
        }
}

print(find_fix(ticker))
