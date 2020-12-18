# I didn't dare to try convolutions in base R given how simple the indexing for GoL, for much
# bigger boards, or much high dim data, this *might* be advisible, since each dimension
# nests another loop - our order of operations is raised one power of N higher (where N is board linear dimension)
# this depends on the efficiency of your conv operator given R is actually fast at
# indexing (when based on index numbers)
#

# I also chose not to bother trimming th board for each iteration
# so the board grows by 2 in each dim for each generation, in part because
# I thought knowing the locations of alive cells might be needed in part 2
#
# However, this would reduce the size of the board and therefore limit your
# checking per loop! For this small a problem the choice of checking would need
# to be careful to ensure that you don't introduce a bottleneck, but for more
# generations the pay off will be much much higher, since the board grows by
# 2 in each dimension in each turn!
#

setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

load_data <- function(filepath,ndim){
        lines <- readLines(filepath)
        if(ndim ==3){
                board <- array(data=0,dim=c(nchar(lines[1]),length(lines),1))
                vals <- strsplit(lines,'')
                for(i in 1:length(lines)){
                        for (j in 1:length(vals[[1]])){
                                board[i,j,1] <- if(vals[[i]][j]=='#'){1}else{0}
                        }
                }
        }else if(ndim ==4){
                board <- array(data=0,dim=c(nchar(lines[1]),length(lines),1,1))
                vals <- strsplit(lines,'')
                for(i in 1:length(lines)){
                        for (j in 1:length(vals[[1]])){
                                board[i,j,1,1] <- if(vals[[i]][j]=='#'){1}else{0}
                        }
                }
        }else{
                stop('we only support 3 and 4 D GoL, no 2D here')
        }
        return(board)
}

apply_gen <- function(board){
        pad_board <- array(data=0,dim=dim(board)+4)
        new_board <- array(data=-1,dim=dim(board)+2) # would allow checking of output
        if(length(dim(board))==3){
                pad_board[3:(dim(board)[1]+2),3:(dim(board)[2]+2),3:(dim(board)[3]+2)]<-board
                for(x in 2:(dim(board)[1]+3)){
                        for(y in 2:(dim(board)[2]+3)){
                                for(z in 2:(dim(board)[3]+3)){
                                        val <- pad_board[x,y,z]
                                        sum_neighs <- sum(pad_board[(x-1):(x+1),(y-1):(y+1),(z-1):(z+1)])-val
                                        if(val==1){
                                                if(sum_neighs%in%c(2,3)){
                                                        new_board[x-1,y-1,z-1]<-1
                                                }else{
                                                        new_board[x-1,y-1,z-1]<-0
                                                }
                                        }else if(val==0){
                                                if(sum_neighs==3){
                                                        new_board[x-1,y-1,z-1]<-1
                                                }else{
                                                        new_board[x-1,y-1,z-1]<-0
                                                }
                                        }else{
                                                stop('your board has an illegal value')
                                        }
                                }
                        }
                } 
        }else if(length(dim(board))==4){
                pad_board[3:(dim(board)[1]+2),3:(dim(board)[2]+2),3:(dim(board)[3]+2),3:(dim(board)[4]+2)]<-board
                for(x in 2:(dim(board)[1]+3)){
                        for(y in 2:(dim(board)[2]+3)){
                                for(z in 2:(dim(board)[3]+3)){
                                        for(w in 2:(dim(board)[3]+3)){
                                                val <- pad_board[x,y,z,w]
                                                sum_neighs <- sum(pad_board[(x-1):(x+1),(y-1):(y+1),(z-1):(z+1),(w-1):(w+1)])-val
                                                if(val==1){
                                                        if(sum_neighs%in%c(2,3)){
                                                                new_board[x-1,y-1,z-1,w-1]<-1
                                                        }else{
                                                                new_board[x-1,y-1,z-1,w-1]<-0
                                                        }
                                                }else if(val==0){
                                                        if(sum_neighs==3){
                                                                new_board[x-1,y-1,z-1,w-1]<-1
                                                        }else{
                                                                new_board[x-1,y-1,z-1,w-1]<-0
                                                        }
                                                }else{
                                                        stop('your board has an illegal value')
                                                }
                                        }
                                }
                        }
                } 
        }else{
               stop('seriously, we only support 3 and 4 D boards, I know, Conway would be sad') 
        }
        return(new_board)
}

boot_cycle <- function(board,n_gens){
        for(n in 1:n_gens){
                board <- apply_gen(board)
        }
        return(board)
}


input <- load_data('day_17_data.txt',3)
print(sum(boot_cycle(input,6)))


input2 <- load_data('day_17_data.txt',4)
print(sum(boot_cycle(input2,6)))