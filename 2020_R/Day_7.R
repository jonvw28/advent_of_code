# NB this was a relatively quickly coded recursion formula which won't cope with
# much bigger data (or at least would take a very long time). I considered developing
# a backwards lookup table and dealing with this as a graph problem, but had
# work to do and so wanted to at least solve the problem at hand. Maybe I will
# come back and build this better in the future.

setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

data <- readLines('day_7_data.txt')

# first get all the bag types
get_bag_list <- function(rules,string){
        list <- NULL
        candidates <- strsplit(rules,string)
        for(i in 1:length(candidates)){
                list <- c(list,candidates[[i]][1])
        }
        # check we have unique rules
        if(length(list)!=length(unique(list))){
                warning('you have multiply defined rules')
        }
        return(list)
}

# map the graph to a one-way graph of intial nestings
parse_rule<- function(rule){
        info <- strsplit(rule,' bags contain')[[1]]
        outer <- info[1]
        nested <- info[2]
        if(grepl('no other',nested)){
                return(info[1])
        }
        output <- info[1]
        inners <- gregexpr('[0-9]{1,} \\w+ \\w+',nested)
        for(j in 1:length(inners[[1]])){
                inner_parts <- strsplit(substring(nested,first = inners[[1]][j], last= (inners[[1]][j]+attr(inners[[1]],'match.length')[j]-1)),' ')
                output <-c(output,paste(inner_parts[[1]][2],inner_parts[[1]][3],sep=' '))
        }
        return(output)
}


create_rules_table <- function(rules,bag_list){
        rule_map <- as.data.frame(matrix(nrow=length(bag_list),ncol=length(bag_list),data=FALSE),row.names = bag_list)
        names(rule_map) <- bag_list
        for(i in 1:length(rules)){
                parsed <- parse_rule(rules[i])
                if(length(parsed)==1){
                        next
                }
                if(!parsed[1]%in%bag_list){
                        stop(paste0('oh no, rule number ',i,' is an issue since we do\'t have ',parsed[1],' in our bag database'))
                }
                for(j in 2:length(parsed)){
                        if(!parsed[j]%in%bag_list){
                                stop(paste0('oh no, rule number ',i,' is an issue since we do\'t have ',parsed[j],' in our bag database'))
                        }else{
                                rule_map[bag_list==parsed[1],parsed[j]] <- TRUE
                        }
                }
        }
        return(rule_map)
}

find_outer_bags <- function(rule_map,colour){
        cands <- rule_map[,colour]
        new_cands <- (cands|apply(rule_map[,cands],1,function(x) sum(x)>=1))
        while(sum(new_cands)>sum(cands)){
                cands <- new_cands
                new_cands <- (cands|apply(rule_map[,cands],1,function(x) sum(x)>=1))
        }
        return(cands)
        
}


bag_list <- get_bag_list(data,' bags contain')
rule_map<- create_rules_table(data,bag_list)
outer_gold <- find_outer_bags(rule_map,'shiny gold')
print(sum(outer_gold))


## part 2

# parse each rule including the number
parse_number<- function(rule){
        info <- strsplit(rule,' bags contain')[[1]]
        outer <- info[1]
        nested <- info[2]
        if(grepl('no other',nested)){
                return(info[1])
        }
        output <- info[1]
        inners <- gregexpr('[0-9]{1,} \\w+ \\w+',nested)
        for(j in 1:length(inners[[1]])){
                inner_parts <- strsplit(substring(nested,first = inners[[1]][j], last= (inners[[1]][j]+attr(inners[[1]],'match.length')[j]-1)),' ')
                output <-c(output,inner_parts[[1]][1],paste(inner_parts[[1]][2],inner_parts[[1]][3],sep=' '))
        }
        return(output)
}

# create and equivalent table from q1, but with numbers rather than bools
create_numbers_table <- function(rules,bag_list){
        rule_map <- as.data.frame(matrix(nrow=length(bag_list),ncol=length(bag_list),data=0),row.names = bag_list)
        names(rule_map) <- bag_list
        for(i in 1:length(rules)){
                parsed <- parse_number(rules[i])
                if(length(parsed)==1){
                        next
                }
                if(!parsed[1]%in%bag_list){
                        stop(paste0('oh no, rule number ',i,' is an issue since we do\'t have ',parsed[1],' in our bag database'))
                }
                for(j in seq(from=3,to=length(parsed),by=2)){
                        if(!parsed[j]%in%bag_list){
                                stop(paste0('oh no, rule number ',i,' is an issue since we do\'t have ',parsed[j],' in our bag database'))
                        }else{
                                rule_map[bag_list==parsed[1],parsed[j]] <- as.numeric(parsed[j-1])
                        }
                }
        }
        return(rule_map)
}

# extract the number and colour of bags within a specific row of the table
get_inner_bags <- function(number_map,row){
        inside <- which(number_map[row,]!=0)
        if(length(inside)==0){
                return(NULL)
        }
        numbers <- unname(number_map[row,inside])
        rownames(numbers) <- c()
        output<- data.frame(rows = inside, nums = t(numbers))
        return(output)
}

number_map<- create_numbers_table(data,bag_list)
test <- get_inner_bags(number_map,which(bag_list=='shiny gold'))

# now iterate to dig down from a top level bag until finished
count_inner_bags <- function(number_map,row){
        inside <- get_inner_bags(number_map,row)
        if(is.null(inside)){
                return(0)
        }else{
                count <- 0
                for(i in 1:nrow(inside)){
                        count <- count + inside[i,2] + inside[i,2]*count_inner_bags(number_map,inside[i,1])
                }
                return(count)
        }
}


number_map<- create_numbers_table(data,bag_list)
count_2 <- count_inner_bags(number_map,which(bag_list=='shiny gold'))
print(count_2)