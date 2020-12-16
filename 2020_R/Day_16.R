setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

load_data <- function(filepath){
        lines<-readLines(filepath)
        line <- 1
        rules <- NULL
        while(lines[line]!=''){
                rules <- c(rules,lines[line])
                line <- line + 1
        }
        tic_lines <- which(grepl('[[0-9]*,]*',lines))
        tics <- read.csv(filepath,skip=tic_lines[1]-1,nrow=1,header = F)
        tics <- rbind(tics,read.csv(filepath,skip=tic_lines[2]-1,header = F))
        return(list(rules,tics))
}

parse_rules <- function(rules){
        field <- character(0)
        allowed <- list()
        rules <- strsplit(rules,': ')
        for(i in 1:length(rules)){
                field <- c(field,rules[[i]][1])
                rngs <- strsplit(rules[[i]][2],' or ')
                ths_vals <- numeric(0)
                for(j in 1:length(rngs[[1]])){
                        vals <- strsplit(rngs[[1]][j],'-')
                                ths_vals <- c(ths_vals,vals[[1]][1]:vals[[1]][2])
                }
                allowed[[i]] <- ths_vals
                
        }
        return(list(field,allowed))
}

convert_rules_to_all_allowed <- function(allowed){
        final_allow <- numeric(0)
        for(i in 1:length(allowed)){
                final_allow <- c(final_allow,allowed[[i]])
        }
        final_allow <- sort(unique(final_allow))
        return(final_allow)
}

get_errors <- function(allow,tics){
        errors<-numeric(0)
        bad_tics <- numeric(0)
        for(i in 1:nrow(tics)){
                mask <- tics[i,]%in%allow
                if(sum(mask)!=ncol(tics)){
                        errors <- c(errors,tics[i,!mask])
                        bad_tics <- c(bad_tics,i)
                }
                
        }
        return(list(errors,bad_tics))
}

get_error_rate <- function(input){
        rules <- parse_rules(input[[1]])
        allowed <- convert_rules_to_all_allowed(rules[[2]])
        errors <- get_errors(allowed,input[[2]])
        return(sum(errors[[1]]))
}

input <- load_data('day_16_data.txt')
print(get_error_rate(input))

# part 2
check_field_options <- function(tics,rules){
        table <- as.data.frame(matrix(nrow=length(rules[[1]]),ncol=length(rules[[1]])+1,data=0))
        names(table)<-c(rules[[1]],'field')
        for(i in 1:length(rules[[1]])){
                table$field[i] <- i
                for(j in 1:length(rules[[1]])){
                        val <- tics[,i]%in%rules[[2]][[j]]
                        if(sum(val)==nrow(tics)){
                                table[i,j] <- 1
                        }
                }
        }
        return(table)
}

# At this point I looked and saw that we have a nice dataset with 1 column valid only for 1 field, 1 for 2, 1 for 3 etc so I suspect nest nicely by just ticking off the nique solution.
# if this is not the case then i will need to programme how to handle when there is
# not a 'simple' single unique option (eg the 2 options for the second col do not include the original uniquely specified column)
# NB this is therefore a solution to the presumed 'simple' case I suspect has been
# set up rather than the more general solver (but I am aware that this is the case)
#
# If I had more time I would programme this more robustly (rather than terminating the algorithm if this happens)
#
# Also i am using R >4.0.0 for SAF=F isn't needed (hurrah) but it thrown in when I remember in case
# this is run on older versions of R
#
resolve_fields <- function(valid_table){
        pairs <- data.frame(field = character(nrow(valid_table)),id=numeric(nrow(valid_table)),stringsAsFactors = FALSE)
        mat <- valid_table[,1:(ncol(valid_table)-1)]
        ids<-valid_table$field
        names <- names(valid_table[,1:(ncol(valid_table)-1)])
        for(i in 1:(nrow(valid_table)-1)){
                counts <- colSums(mat)
                if(sum(counts==0)!=0){
                        stop('you\'ve found a field which is not valid for any option')
                }else if(sum(counts==1)==0){
                        stop('there are no unique remaining fields, this algorithm needs to be made more robust, to explore all valid options')
                }else if(sum(counts==1)>1){
                        stop('there are multiple unique remaining fields, this algorithm needs to be made more robust - to handle these equentially rather assume this is a simple elimination')
                }else{
                        un_fld <- which(counts==1)
                        fld_id <- which(mat[,un_fld]==1)
                        pairs[i,1] <- names[un_fld]
                        pairs[i,2] <- ids[fld_id]
                }
                if(i<(nrow(valid_table)-1)){
                        mat <- mat[,-un_fld]
                        mat <- mat[-fld_id,]
                        ids <- ids[-fld_id]
                        names <- names[-un_fld]
                }else if(i==nrow(valid_table)-1){#since R data types make this a vector at this point
                        mat <- mat[,-un_fld]
                        mat <- mat[-fld_id]
                        ids <- ids[-fld_id]
                        names <- names[-un_fld]
                }
                
        }# NB loop stops 1 short for a similar issue, colSums won't play nice once our datatype is resolved, why can't I keep a data.frame 1x1 by default...
        pairs[nrow(valid_table),1] <- names
        pairs[nrow(valid_table),2] <- ids
        return(pairs)
}

get_field_multiplied <- function(input){
        rules <- parse_rules(input[[1]])
        allowed <- convert_rules_to_all_allowed(rules[[2]])
        errors <- get_errors(allowed,input[[2]])
        if(length(errors[[2]])!=0){
        
                input[[2]] <- input[[2]][-errors[[2]],]
        }
        valid_table <- check_field_options(input[[2]],rules)
        field_map <- resolve_fields(valid_table) # phew that worked - the problem is nice
        mask <- grepl('departure',field_map$field)
        return(prod(input[[2]][1,field_map$id[mask]]))
}

format(get_field_multiplied(input),digits=20)
