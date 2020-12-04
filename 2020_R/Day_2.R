setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

data <- read.csv('day_2_data.txt',sep = ':',header=F)
names(data) <- c('form','psd')
tally <- logical(nrow(data))

rules <- strsplit(data$form,' ')
for(i in 1:nrow(data)){
        if(rules[[i]][2]=='1'){
                warning(paste0('using 1 was silly because of row ',i))
        }
}
data[,2]<-sub(' ','',data[,2])
data[,2] <- paste0(1,data[,2],1) # pad each end with a 1 to ensure strsplit works (don't do strings in R kids)
for(i in 1:nrow(data)){
        if(length(rules[[i]])!=2){
                warning(paste0('row ',i,' has unconventional form'))
        }
        limits <- strsplit(rules[[i]][1],'-')
        reps <- length(strsplit(data[i,2],rules[[i]][2])[[1]])-1
        if((as.numeric(limits[[1]][1])<=reps)&(as.numeric(limits[[1]][2])>=reps)){
                tally[i] <- TRUE
        }
}


### Part 2
data2 <- read.csv('day_2_data.txt',sep = ':',header=F)
names(data2) <- c('form','psd')

rules2 <- strsplit(data2$form,' ')
data2[,2]<-sub(' ','',data2[,2])
tally2 <- logical(nrow(data))
for(i in 1:nrow(data2)){
        if(length(rules2[[i]])!=2){
                warning(paste0('row ',i,' has unconventional form'))
        }
        spots <- strsplit(rules2[[i]][1],'-')
        test1 <- substring(data2[i,2],spots[[1]][1],spots[[1]][1])==rules2[[i]][2]
        test2 <- substring(data2[i,2],spots[[1]][2],spots[[1]][2])==rules2[[i]][2]
        if((sum(test1)+sum(test2))==1){
                tally2[i] <- TRUE
        }
}