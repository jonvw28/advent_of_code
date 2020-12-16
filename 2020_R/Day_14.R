# This one is going to annoy me a lot. using only base R I can only use 32-bit integers
# if only we had a long int class in base R
#
# Thankfully 2^53 is the first time 2^n + 1 == 2^n, though 2^53 - 1 != 2^53
# so we could risk up to 53 bit integers, but this is only 36 so we should
# be ok, if long...
#
# NB R uses signed 32 bit ints so we have to unpack 5 not 4 bit
#

setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

parse_input <- function(filepath){
        data <- strsplit(readLines(filepath),' = ')
        msk <- character(0)
        adrs <- numeric(0)
        val <- numeric(0)
        map <- numeric(0)
        count <- 0
        for(i in 1:length(data)){
                if(data[[i]][1]=='mask'){
                        msk <- c(msk,data[[i]][2])
                        count <- count + 1
                }else if(grepl('mem\\[[0-9]*\\]',data[[i]][1])){
                        adrs <- c(adrs,as.numeric(substring(data[[i]][1],first = 5,last=nchar(data[[i]][1])-1)))
                        val <- c(val,as.numeric(data[[i]][2]))
                        map <- c(map,count)
                }
        }
        wrts <- data.frame(adrs = adrs, map=map, val= val)
        return(list(msk,wrts))
}

drop_overwritten <- function(val_tbl){
        adrs <- val_tbl$adrs
        mask <- !logical(length=length(adrs))
        written <- numeric(0)
        for(j in length(adrs):1){
                if(adrs[j]%in%written){
                        mask[j]<-F
                }else{
                        written <- c(written,adrs[j])
                }
        }
        val_tbl <- val_tbl[mask,]
        return(val_tbl)
}

bit_36_to_32 <- function(val_tbl){
        vals <- val_tbl$val
                for(i in 0:4){
                        val_tbl[,paste0('bit_',35-i)]<- 0
                        mask <- vals>=2^(35-i)
                        val_tbl[mask,paste0('bit_',35-i)]<- 1
                        vals[mask]<- vals[mask]-2^(35-i)
                }
        val_tbl$val <- as.integer(vals)
        return(val_tbl)
}

bit_32_to_36 <-function(val_tbl){
        vals <- val_tbl$val
        for(i in 0:4){
                mask <- val_tbl[,paste0('bit_',35-i)]==1
                vals[mask]<- vals[mask]+2^(35-i)
        }
        val_tbl$val_36 <- vals
        return(val_tbl)
}

parse_masks <- function(masks){
        bit_35 <- character(length(masks))
        bit_34 <- character(length(masks))
        bit_33 <- character(length(masks))
        bit_32 <- character(length(masks))
        bit_31 <- character(length(masks))
        mask_1 <- integer(length(masks))
        mask_0 <- integer(length(masks))
        for(i in 1:length(masks)){
                map <- strsplit(masks[i],'')[[1]]
                bit_35[i] <- map[1]
                bit_34[i] <- map[2]
                bit_33[i] <- map[3]
                bit_32[i] <- map[4]
                bit_31[i] <- map[5]
                for(j in 30:0){
                        if(map[36-j]=='0'){
                                mask_0[i] <- mask_0[i]+2^j
                        }else if(map[36-j]=='1'){
                                mask_1[i] <- mask_1[i]+2^j
                        }else if(map[36-j]=='X'){
                                next
                        }else{
                                print(c(i,j))
                                stop('you have a broken bitmask')
                        }
                }
        }
        masks <- data.frame(bit_35=bit_35,
                            bit_34=bit_34,
                            bit_33=bit_33,
                            bit_32=bit_32,
                            bit_31=bit_31,
                            mask_1=mask_1,
                            mask_0=mask_0)
        return(masks)
}

apply_mask <- function(value,msk){
        for(j in 1:5){
                if(msk[,paste0('bit_',36-j)]=='0'){
                        value[,paste0('bit_',36-j)]=0
                }else if(msk[,paste0('bit_',36-j)]=='1'){
                        value[,paste0('bit_',36-j)]=1
                }else if(msk[,paste0('bit_',36-j)]=='X'){
                        next
                }else{
                        stop(paste0('your first 4 bits are faulty for the map ',mask))
                }
        }
        val <- bitwOr(as.integer(value$val),msk$mask_1)
        val <-bitwAnd(val,2^31-1-msk$mask_0)
        value$val <- val
        return(value)
}

apply_masks_all <- function(input){
        masks <- input[[1]]
        values <- input[[2]]
        for(i in 1:nrow(values)){
                mask <- masks[values$map[i],]
                values[i,] <- apply_mask(values[i,],mask)
        }
        return(list(masks,values))
}



input <- parse_input('day_14_data.txt')
input[[2]] <- drop_overwritten(input[[2]])
input[[1]] <- parse_masks(input[[1]])
input[[2]] <- bit_36_to_32(input[[2]])
output <- apply_masks_all(input)
output[[2]] <- bit_32_to_36(output[[2]])
format(sum(output[[2]]$val_36),digits=20)
rm(input,output)

# All this specific unpacking and repacking was built for the original problem and
# just my luck, the second part is different as I don't have the heart to go back and
# break something that works, I will instead in effect, copy the functions and then
# edit them to solve problem 2
#
# Were I not stubborn to prove I can do this with base R, I would be loading a
# package to allow me to use int 64 which would be a LOT easier
#

adrs_to_bits <- function(adrs){
        res <- numeric(36)
        for(i in 1:36){
                if(adrs>=2^(36-i)){
                        res[i] <- 1
                        adrs <- adrs - 2^(36-i)
                }
        }
        if(adrs!=0){
                stop('this implementation of int-36 is bad')
        }
        return(res)
}

mask_adrs <- function(adrs,mask){
        map <- strsplit(mask,'')
        adrs <- matrix(adrs,nrow=1,ncol=36)
        for(i in 1:36){
                if(map[[1]][i]=='0'){
                        next
                }else if(map[[1]][i]=='1'){
                       adrs[,i] <- 1
                }else if(map[[1]][i]=='X'){
                        tmp <- adrs
                        adrs[,i]<- 1
                        tmp[,i]<-0
                        adrs <- rbind(adrs,tmp)
                        rm(tmp)
                }else{
                        stop('that is not a valid mask')
                }
        }
        final_ads <- numeric(0)
        for(j in 1:nrow(adrs)){
                final_ads <- c(final_ads,sum(2^(35:0)*adrs[j,]))
        }
        return(final_ads)
}

# at this pint we are so close, and yet so far. We need a clever way to have
# sparse vector of values as there is no way R will handle 2^36 -1 addresses
# another time I really ought to use a package but want to prove I can do
# this without doing so
run_prog_2 <- function(input){
        out_vals <- NULL
        for(i in 1:nrow(input[[2]])){
                test <- adrs_to_bits(input[[2]]$adrs[i])
                ad_list <- mask_adrs(adrs_to_bits(input[[2]]$adrs[i]),input[[1]][input[[2]]$map[i]])
                for(ad in ad_list){
                        if(!ad%in%out_vals[,1]){
                                out_vals<-rbind(out_vals,c(ad,input[[2]]$val[i]))
                        }else{
                                out_vals[out_vals[,1]==ad,2]<-input[[2]]$val[i]
                        }
                }
        }
        return(out_vals)
}

input2 <- parse_input('day_14_data.txt')
output2 <- run_prog_2(input2)
format(sum(output2[,2]),digits=20)

# that wasn't so bad