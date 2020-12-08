# this one was done very quickly, but does the job. It would be easy to instead write this
# terms of functions for computing the count for each group instead of represted nesting, but
# given the simplicity of the inner part and limited scope of the data this solution
# does the job even if it could be made a little prettier

setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

# take some parts from day 4
data <- readLines('day_6_data.txt')
breaks <- which(data =='')
breaks <- c(0,breaks,length(data)+1)
tally <- 0 
for(i in 2:(length(breaks))){
        str <- ''
        for(j in (breaks[i-1]+1):(breaks[i]-1)){
                str<- paste0(str,data[j])
        }
        # just use the regex to match each letter in the merged string for each group
        for(let in letters){
                tally <- tally + grepl(let,str)
        }
}
print(tally)

# part 2
tally2 <- 0 
for(i in 2:(length(breaks))){
        pres <- logical(length=26)
        for(k in 1:26){
                 pres[k] <- grepl(letters[k],data[breaks[i-1]+1])
        }
        if(breaks[i]>breaks[i-1]+2){
                for(j in (breaks[i-1]+2):(breaks[i]-1)){
                        for(k in 1:26){
                                pres[k] <- grepl(letters[k],data[j])&pres[k]
                        }
                }
        }
        tally2 <- tally2 + sum(pres)
}
print(tally2)