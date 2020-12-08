setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

# strings aren't s bad this time
data <- readLines('day_4_data.txt')
breaks <- which(data =='')
breaks <- c(0,breaks,length(data)+1)
tally <- 0 
for(i in 2:(length(breaks))){
        str <- ''
        for(j in (breaks[i-1]+1):(breaks[i]-1)){
                str<- paste(str,data[j], sep=' ')
        }
        num_field <- 0+grepl('byr:',str)+grepl('iyr:',str)+grepl('eyr:',str)+grepl('hgt:',str)+grepl('hcl:',str)+grepl('ecl:',str)+grepl('pid:',str)
        if(num_field == 7){
                tally <- tally + 1
        }
}
print(tally)


# Part 2
tally2 <- 0 
for(i in 2:length(breaks)){
        str <- ''
        for(j in (breaks[i-1]+1):(breaks[i]-1)){
                str<- paste(str,data[j], sep=' ')
        }
        num_field <- 0+grepl('byr:',str)+grepl('iyr:',str)+grepl('eyr:',str)+grepl('hgt:',str)+grepl('hcl:',str)+grepl('ecl:',str)+grepl('pid:',str)
        if(num_field != 7){
                next
        }
        # Test using regex sadness
        
        #BYR
        byr_info <-  regexpr('byr:[0-9]*',str)
        if(attr(byr_info,'match.length')!= 8){
                next
        }
        byr <- as.numeric(substring(str,first = byr_info+4, last= byr_info+attr(byr_info,'match.length')-1))
        if(byr<1920 | byr>2002){
                next
        }
        
        #IYR
        iyr_info <-  regexpr('iyr:[0-9]*',str)
        if(attr(iyr_info,'match.length')!= 8){
                next
        }
        iyr <- as.numeric(substring(str,first = iyr_info+4, last= iyr_info+attr(iyr_info,'match.length')-1))
        if(iyr<2010 | iyr>2020){
                next
        }
        
        #EYR
        eyr_info <-  regexpr('eyr:[0-9]*',str)
        if(attr(eyr_info,'match.length')!= 8){
                next
        }
        eyr <- as.numeric(substring(str,first = eyr_info+4, last= eyr_info+attr(eyr_info,'match.length')-1))
        if(eyr<2020 | eyr>2030){
                next
        }
        
        #hgt
        hgt_info <-  regexpr('hgt:[0-9]*(cm|in)',str)
        if(attr(eyr_info,'match.length')== -1){
                next
        }
        hgt <- as.numeric(substring(str,first = hgt_info+4, last= hgt_info+attr(hgt_info,'match.length')-3))
        unit <- substring(str,first = hgt_info+attr(hgt_info,'match.length')-2, last= hgt_info+attr(hgt_info,'match.length')-1)
        if(unit=='cm'){
                if(hgt<150|hgt>193){
                        next
                }
        }else if(unit=='in'){
                if(hgt<59|hgt>76){
                        next
                }
        }else{next}
        

        #HCL
        hcl_info <-  regexpr('hcl:#[a-z0-9]*',str)
        if(attr(hcl_info,'match.length')!= 11){
                next
        }

        #ECL
        ecl_info <-  regexpr('ecl:[a-z]*',str)
        if(attr(ecl_info,'match.length')!= 7){
                next
        }
        ecl <- substring(str,first = ecl_info+4, last= ecl_info+attr(ecl_info,'match.length')-1)
        if(!(ecl %in% c('amb','blu','brn','gry','grn','hzl','oth'))){
                next
        }
        
        #PID
        pid_info <-  regexpr('pid:[0-9]*',str)
        if(attr(pid_info,'match.length')!= 13){
                next
        }
        

        tally2 <- tally2+1
}
print(tally2)