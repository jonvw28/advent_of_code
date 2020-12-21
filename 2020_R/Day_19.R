setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

load_data <- function(filepath){
        data <- readLines(filepath)
        split <- which(data=='')
        rules <- character(split-1)
        prs_rl <- strsplit(data[1:(split-1)],': ')
        for(i in 1:length(prs_rl)){
                idx <- as.numeric(prs_rl[[i]][1])
                if(rules[idx+1]!=''){
                        warning(paste0('The rule on line ',i,' is overwiting an earlier rule'))
                }
                if(idx>(split-1)){
                        stop('you have higher rule numbers than actual rules')
                }
                rules[idx+1] <- prs_rl[[i]][2]
        }
        rules <- gsub('\"','',rules)
        rules <- gsub(' ','  ',rules)
        rules <- gsub(' \\| ','\\|',rules)
        rules <- paste0(' ',rules,' ')
        #rules<-strsplit(rules,' \\| ')
        messages <- data[(split+1):length(data)]
        return(list(rules,messages))
}
# I checked the dependency and rule 0 needs knowledge of all rules, so only solution is to build the rules explicitly

# BUT we can be efficient and not build all the rules in one go, until we encounter a split path we can simply stick all options
# for each part independently (ie for each message, check if the first part matches
# an option for the opening part of our tree, then second and so on)

split_rule <- function(rules,target){
        checked <- logical(length(rules))
        cand <- rules[[target+1]]
        if(length(cand)>1){
                warning('this rule is already made of options, returning the rule')
                return(cand)
        }
        parts <- as.numeric(strsplit(gsub('  ',' ',substring(cand,2,nchar(cand)-1)),' ')[[1]])
        Flag <- TRUE
        loc <- 1
        while(Flag){
                if(loc>length(parts)){
                        Flag <- FALSE
                        next
                }
                rule <- parts[loc]
                cand <- rules[[parts[loc]+1]]
                if(length(cand)>1){
                        loc <- loc + 1
                        next
                }else if(cand %in% c(' a ',' b ')){
                        loc <- loc + 1
                }else{
                        new_parts <- as.numeric(strsplit(gsub('  ',' ',substring(cand,2,nchar(cand)-1)),' ')[[1]])
                        if(loc == 1){
                                parts <- c(new_parts,parts[2:length(parts)])
                        }else if (loc == length(parts)){
                                parts <- c(parts[1:(length(parts)-1)],new_parts)
                        }else{
                                parts <- c(parts[1:(loc-1)],new_parts,parts[(loc+1):length(parts)])
                        }
                }
        }
        return(parts)
}

parse_regex <- function(rules,target){
        checked <- logical(length(rules))
        cur <- grep('[a-b]',rules)
        rules[cur] <- gsub(' ','',rules[cur])
        cur <- cur-1
        nxt <- numeric(0)
        expr <- ''
        for(rul in cur){
                rules[rul+1] <- paste0('(',rules[rul+1],')')
                rules <- gsub(paste0(' ',rul,' '),rules[rul+1],rules)
                checked[rul+1] <- TRUE
                if(checked[target+1]){
                        return(paste0('^',rules[target+1],'$'))
                }
        }
        nxt <- c(nxt,which((!grepl('[0-9]',rules))&!checked))
        nxt <- sort(unique(nxt))
        while(length(nxt !=0)){
                cur <- nxt-1
                nxt <- numeric(0)
                for(rul in cur){
                        rules[rul+1] <- paste0('(',rules[rul+1],')')
                        rules <- gsub(paste0(' ',rul,' '),rules[rul+1],rules)
                        checked[rul+1] <- TRUE
                        if(checked[target+1]){
                                return(paste0('^',rules[target+1],'$'))
                        }
                }
                nxt <- c(nxt,which((!grepl('[0-9]',rules))&!checked))
                nxt <- sort(unique(nxt))
        }
        return(paste0('^',rules[target+1],'$'))
}

input <- load_data('day_19_data.txt')
print(sum(grepl(parse_regex(input[[1]],0),input[[2]])))

# part 2

# First check how rule 0 depends on the rules that might change

split_rule <- function(rules,target){
        checked <- logical(length(rules))
        if(grepl('\\|',rules[target+1])){
                warning('this rule is already made of options, returning the rule')
                return(rules[target+1])
        }
        cand <- strsplit(rules[target+1],'  ')
        parts <- as.numeric(gsub(' ','',cand[[1]]))
        Flag <- TRUE
        loc <- 1
        while(Flag){
                if(loc>length(parts)){
                        Flag <- FALSE
                        next
                }
                rule <- parts[loc]
                cand <- rules[parts[loc]+1]
                if(grepl('\\|',cand)){
                        loc <- loc + 1
                        next
                }else if(cand %in% c(' a ',' b ')){
                        loc <- loc + 1
                }else{
                        new_parts <- as.numeric(gsub(' ','',strsplit(cand,'  ')[[1]]))
                        #new_parts <- as.numeric(strsplit(gsub('  ',' ',substring(cand,2,nchar(cand)-1)),' ')[[1]])
                        if(loc == 1){
                                parts <- c(new_parts,parts[2:length(parts)])
                        }else if (loc == length(parts)){
                                parts <- c(parts[1:(length(parts)-1)],new_parts)
                        }else{
                                parts <- c(parts[1:(loc-1)],new_parts,parts[(loc+1):length(parts)])
                        }
                        loc <- loc + 1
                }
        }
        return(parts)
}
print(split_rule(input[[1]],0))
# so we do only care about 42 and 31 which appear in the new rules anyway
# rule 8 is now 8: 42 | 42 8, ie the start of rule 0 can be n times rule 42
# similarly, 13: 42 31 | 42 13 31 so bracket itself withing 42 and 31.
print(split_rule(input[[1]],8))
print(split_rule(input[[1]],11))

# SO we just need minimum 2 * 42 and 1* 31 in that order
count_recur <- function(rules,messages){
        expr_31 <- parse_regex(rules,31)
        expr_42 <- parse_regex(rules,42)
        
        part_2_reg_0_0 <- paste0(substring(expr_42,1,nchar(expr_42)-1),'{1}(',substring(expr_42,2,nchar(expr_42)-1),substring(expr_31,2,nchar(expr_31)-1),'){1}$')
        min_leng <- nchar(grep(part_2_reg_0_0,messages,value = T)[1])
        part_2_reg_1_0 <- paste0(substring(expr_42,1,nchar(expr_42)-1),'{2}(',substring(expr_42,2,nchar(expr_42)-1),substring(expr_31,2,nchar(expr_31)-1),'){1}$')
        leng_42 <- nchar(grep(part_2_reg_1_0,messages,value = T)[2]) - min_leng
        part_2_reg_0_1 <- paste0(substring(expr_42,1,nchar(expr_42)-1),'{1}(',substring(expr_42,2,nchar(expr_42)-1),'){2}(',substring(expr_31,2,nchar(expr_31)-1),'){2}$')
        leng_31 <- nchar(grep(part_2_reg_0_1,messages,value = T)[2]) - min_leng
        max_leng <- max(nchar(messages))
        delta <- max_leng-min_leng
        tot <- 0 
        for(i in 1:(delta/leng_31)){
                for(j in 1:((delta-i*leng_31)/leng_42)){
                        cur_reg <- paste0(substring(expr_42,1,nchar(expr_42)-1),'{',j,'}(',substring(expr_42,2,nchar(expr_42)-1),'){',i,'}(',substring(expr_31,2,nchar(expr_31)-1),'){',i,'}$')
                        tot <- tot + sum(grepl(cur_reg,messages))
                        }
                }
        return(tot)
}
print(count_recur(input[[1]],input[[2]]))
