setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

parse_input <- function(filepath){
        data <- readLines(filepath)
        data <- gsub('\\(','',data)
        data <- gsub('\\)','',data)
        ingr <- NULL
        parts <- strsplit(data,' contains ')
        for(i in 1:length(data)){
               ingr[[i]] <- strsplit(parts[[i]][1],' ')[[1]]
               ths_al <- strsplit(parts[[i]][2],', ')[[1]]
               if(i==1){
                       alls <- matrix(nrow=length(data),ncol=length(ths_al),data=0)
                       alls <- as.data.frame(alls)
                       names(alls) <- ths_al
                       alls[1,] <- 1
               }else{
                       for(all in ths_al){
                               alls[i,all] <- 1
                       }
               }
        }
        alls[is.na(alls)]<-0
        return(list(ingr,alls))
}


map_allergens <- function(input){
        output <- NULL
        un_all <- character(0)
        for(i in 1:ncol(input[[2]])){
                ids <- which(input[[2]][i]==1)
                cands <- input[[1]][[ids[1]]]
                if(length(ids)>1){
                        for(id in ids[2:length(ids)]){
                                cands <- intersect(cands,input[[1]][[id]])
                        }
                }
                output[[names(input[[2]])[i]]]<-cands
                if(length(cands)==1){
                        un_all <- c(un_all,cands)
                }
        }
        new_un <- un_all
        for(name in names(output)){
                if(length(output[[name]])>1){
                        cands <- setdiff(output[[name]],un_all)
                        if(length(cands)==1){
                                new_un <- unique(c(new_un,cands))
                        }
                        output[[name]] <- cands        
                }
                
        }
        
        while(length(new_un)!=length(un_all)){
                un_all <- new_un
                for(name in names(output)){
                        if(length(output[[name]])>1){
                                cands <- setdiff(output[[name]],un_all)
                                if(length(cands)==1){
                                        new_un <-  unique(c(new_un,cands))
                                }
                                output[[name]] <- cands        
                        }
                }
        }
        allergens <- matrix(ncol=2,nrow=length(output))
        allergens <- as.data.frame(allergens)
        names(allergens) <- c('allergen','value')
        for(i in 1:length(output)){# NB this is because I checked we get a neat one to one mapping
                allergens[i,1]<-names(output[i])
                allergens[i,2]<- output[[i]]
        }
        return(allergens)
}

count_other_ings <- function(input,allergens){
        tot <- 0
        for(i in 1:length(input[[1]])){
                tot <- tot + sum(!input[[1]][[i]]%in%allergens[,2])
        }
        return(tot)
}

parse_list <- function(allergens){
        order <- sort(allergens[,1],index.return=TRUE)
        ing_list <- paste(as.character(allergens[order$ix,2]),sep=',',collapse = ',')
        return(ing_list)
}

input <- parse_input('day_21_data.txt')
allergens <- map_allergens(input)
print(count_other_ings(input,allergens))
print(parse_list(allergens))