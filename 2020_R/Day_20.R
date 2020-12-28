setwd("D:/jon/Documents/Cache/advent_of_code/2020_R")

load_data <- function(filepath){
        data <- readLines(filepath)
        tiles <- list()
        split <- which(data=='')
        count <- 1
        ids<-numeric(0)
        for(end in seq(from=12,to=length(data)+1,by=12)){
                id <- regexpr('[0-9]{1,}',data[end-11])
                id <- substring(data[end-11],first = id,last=attr(id,'match.length')+id-1)
                board <- matrix(nrow=10,ncol=10,data='')
                for(i in 1:10){
                        board[i,]<-strsplit(data[end-11+i],'')[[1]]
                }
                tiles[[count]] <- board
                ids[count]<-id
                count <- count+1
        }
        sorted <- sort(ids,index.return=T)
        return(list(tiles[sorted$ix],as.numeric(sorted$x)))
}

edge_to_bin <- function(edge){
        bin <- sum((edge=='#')*2^(0:9))
        bin2 <- sum((edge=='#')*2^(9:0))
        return(c(bin,bin2))
}

board_sig <- function(board){
        sigs <- matrix(nrow=4,ncol=2,data=0)
        sigs[1,] <- edge_to_bin(board[1,])
        sigs[2,] <- edge_to_bin(board[,10])
        sigs[3,] <- edge_to_bin(board[10,10:1])
        sigs[4,] <- edge_to_bin(board[10:1,1])
        return(sigs)
}

compile_sigs <- function(tiles){
        sigs <- NULL
        ids <- NULL
        for(i in 1:length(tiles[[1]])){
                ths_sg <- board_sig(tiles[[1]][[i]])
                sigs <- c(sigs,ths_sg[1,],ths_sg[2,],ths_sg[3,],ths_sg[4,])
                ids <- c(ids,rep(tiles[[2]][i],times=8))
        }
        return(cbind(as.numeric(sigs),as.numeric(ids)))
}

find_corners <- function(sigs){
        uni <- sigs[,1] %in% as.numeric(names(table(sigs[,1])[table(sigs[,1])==1]))
        unids <- sigs[uni,2]
        corns <- as.numeric(names(table(unids)[table(unids)==4]))
        return(corns)
}

tiles <-  load_data('day_20_data.txt')
format(prod(find_corners(compile_sigs(tiles))),digits=20)

# Part 2 - needs me to resolve the board, so let's strip the layers off

find_edges <- function(sigs){
        uni <- sigs[,1] %in% as.numeric(names(table(sigs[,1])[table(sigs[,1])==1]))
        unids <- sigs[uni,2]
        edgs <- as.numeric(names(table(unids)[table(unids)==2]))
        return(edgs)
}

find_layers <- function(sigs){
        layers <- list()
        crns <- find_corners(sigs)
        edgs <- find_edges(sigs)
        layers[[1]] <- list(crns,edgs)
        mask <- sigs[,2] %in% c(crns,edgs)
        new_sigs <- sigs[!mask,]
        cnt <- 2
        while(nrow(new_sigs)>4*8){
                crns <- find_corners(new_sigs)
                edgs <- find_edges(new_sigs)
                layers[[cnt]] <- list(crns,edgs)
                mask <- new_sigs[,2] %in% c(crns,edgs)
                new_sigs <- new_sigs[!mask,]
                cnt <- cnt + 1
        }
        layers[[cnt]] <- unique(new_sigs[,2])
        return(layers)
}

transform_grid <- function(grid,sigs,type){ # we could include all 8 (well 7) options, but this is already a lot of code
        if(type=='flip'){
                new_grid <- grid[,ncol(grid):1]
                new_sigs <- sigs
                new_sigs[3:4,1] <- sigs[8:7,1]
                new_sigs[7:8,1] <- sigs[4:3,1]
                new_sigs[1:2,1] <- sigs[2:1,1]
                new_sigs[5:6,1] <- sigs[6:5,1]
        }else if(type=='rotate'){
                new_grid <- grid
                new_sigs <- sigs
                for(i in 1:nrow(grid)){
                        new_grid[,nrow(grid)+1-i] <- grid[i,]
                }
                new_sigs[3:8,1] <- sigs[1:6,1]
                new_sigs[1:2,1] <- sigs[7:8,1]
        }else{
                stop('You can only flip or rotate the grid')
        }
        return(list(new_grid,new_sigs))
}

align_grid <- function(grid,sigs,init,target){
        if(init==target){
                warning('you are already done')
                return(list(grid,sigs))
        }
        if(init<1 | init>8 | target<1 | target>8){
                stop('One of your locations is wrong')
        }
        sig <- sigs[init,1]
        if(init%%2!=target%%2){
                tmp <- transform_grid(grid,sigs,'flip')
                grid <- tmp[[1]]
                sigs <- tmp[[2]]
                rm(tmp)
        }
        cur <- which(sigs[,1]==sig)
        while(cur!=target){
                tmp <- transform_grid(grid,sigs,'rotate')
                grid <- tmp[[1]]
                sigs <- tmp[[2]]
                rm(tmp)
                cur <- which(sigs[,1]==sig)
        }
        return(list(grid,sigs))
}

init_pic <- function(layers,tiles,sigs){
        seeds <- layers[[length(layers)]]
        tl_sigs <- sigs[sigs[,2]%in%seeds,]
        if(length(seeds)==1){
                board <- tiles[[1]][[which(tiles[[2]]==seeds)]]
                init_brd <- board[2:9,2:9]
                edges <- list(tl_sigs[2,1],tl_sigs[4,1],tl_sigs[6,1],tl_sigs[8,1])
        }else if(length(seeds==4)){
                joins <- as.numeric(names(table(tl_sigs)[table(tl_sigs)==2]))
                init_brd <- matrix(nrow=16,ncol=16,data='')
                edges <- list(numeric(2),numeric(2),numeric(2),numeric(2))
                ths_brd <- tiles[[1]][[which(tiles[[2]]==seeds[1])]]
                ths_sigs <- tl_sigs[1:8,]
                orient <- unique(ceiling(which(ths_sigs[,1]%in%joins)/2))
                while(any(orient!=c(2,3))){
                        tmp <- transform_grid(ths_brd,ths_sigs,'rotate')
                        ths_brd <- tmp[[1]]
                        ths_sigs <- tmp[[2]]
                        orient <- unique(ceiling(which(ths_sigs[,1]%in%joins)/2))
                        rm(tmp)
                }
                init_brd[1:8,1:8]<-ths_brd[2:9,2:9]
                edges[[1]][1]<- ths_sigs[2,1]
                edges[[4]][2]<- ths_sigs[8,1]
                chkd <- c(1)
                # Now build on this
                for(i in 2:4){
                        ths_sg <- ths_sigs[2*i-1,1]
                        new_id <- ceiling(which(tl_sigs[1:32,1]==ths_sg)/8)
                        new_id <- new_id[!new_id%in%chkd]
                        ths_brd <- tiles[[1]][[which(tiles[[2]]==seeds[new_id])]]
                        ths_sigs <- tl_sigs[8*(new_id-1)+(1:8),]
                        ths_loc <- which(ths_sigs[,1]==ths_sg)
                        if(i==2){
                                trgt <- 8
                        }else{
                                trgt <- 2*i-4
                        }
                        tmp <- align_grid(ths_brd,ths_sigs,ths_loc,trgt)
                        ths_brd <- tmp[[1]]
                        ths_sigs <- tmp[[2]]
                        if(i==2){
                                init_brd[1:8,9:16]<-ths_brd[2:9,2:9]
                                edges[[1]][2]<-ths_sigs[2,1]
                                edges[[2]][1]<-ths_sigs[4,1]
                        }else if(i==3){
                                init_brd[9:16,9:16]<-ths_brd[2:9,2:9]
                                edges[[2]][2]<-ths_sigs[4,1]
                                edges[[3]][1]<-ths_sigs[6,1]
                        }else{
                                init_brd[9:16,1:8]<-ths_brd[2:9,2:9]
                                edges[[3]][2]<-ths_sigs[6,1]
                                edges[[4]][1]<-ths_sigs[8,1]
                        }
                        rm(tmp)
                        chkd <- sort(unique(c(chkd,new_id)))
                }

                
                
        }else{
                stop('seems your grid is not square')
        }
        return(list(init_brd,edges))
}

add_layer <- function(pic,layers,lyr_no,tiles,sigs){
        new_pic <- matrix(nrow=nrow(pic[[1]])+16,ncol=ncol(pic[[1]])+16,data='')
        new_pic[9:(nrow(pic[[1]])+8),9:(ncol(pic[[1]])+8)] <- pic[[1]]
        edges <- list(numeric(ncol(pic[[1]])/8+2),numeric(nrow(pic[[1]])/8+2),numeric(ncol(pic[[1]])/8+2),numeric(nrow(pic[[1]])/8+2))
        seeds <- layers[[lyr_no]][[2]]
        tl_sigs <- sigs[sigs[,2]%in%seeds,]
        corner_ln <- numeric(4)
        for(side in 1:4){
                for(i in 1:length(pic[[2]][[side]])){
                        th_sd <- pic[[2]][[side]][i]
                        new_id <- ceiling(which(tl_sigs[,1]==th_sd)/8)
                        ths_brd <- tiles[[1]][[which(tiles[[2]]==seeds[new_id])]]
                        ths_sigs <- tl_sigs[8*(new_id-1)+(1:8),]
                        ths_loc <- which(ths_sigs[,1]==th_sd)
                        trgt <- (2*side+3)%%8
                        tmp <- align_grid(ths_brd,ths_sigs,ths_loc,trgt)
                        ths_grd <- tmp[[1]]
                        ths_sigs <- tmp[[2]]
                        if(side==1){
                                rows <- 1:8
                                cols <- 8*i+(1:8)
                        }else if(side==2){
                                rows <- 8*i+(1:8)
                                cols <- ncol(pic[[1]])+8+(1:8)
                        }else if(side==3){
                                rows <- nrow(pic[[1]])+8+(1:8)
                                cols <- 8*(length(pic[[2]][[side]])-i+1)+(1:8)
                        }else{
                                rows <- 8*(length(pic[[2]][[side]])-i+1)+(1:8)
                                cols <- 1:8
                        }
                        new_pic[rows,cols] <- ths_grd[2:9,2:9]
                        edges[[side]][i+1] <- ths_sigs[2*side,1]
                        if(i == 1){
                                if(side==1){
                                        loc <- 8
                                }else{
                                        loc <- 2*(side-1)
                                }
                                corner_ln[side] <- ths_sigs[loc,1]
                        }
                }
        }
        seeds <- layers[[lyr_no]][[1]]
        tl_sigs <- sigs[sigs[,2]%in%seeds,]
        for(corn in 1:4){
                th_sd <- corner_ln[corn]
                new_id <- ceiling(which(tl_sigs[,1]==th_sd)/8)
                ths_brd <- tiles[[1]][[which(tiles[[2]]==seeds[new_id])]]
                ths_sigs <- tl_sigs[8*(new_id-1)+(1:8),]
                ths_loc <- which(ths_sigs[,1]==th_sd)
                trgt <- (2*corn+1)%%8
                tmp <- align_grid(ths_brd,ths_sigs,ths_loc,trgt)
                ths_grd <- tmp[[1]]
                ths_sigs <- tmp[[2]]
                if(corn==1){
                        rows <- 1:8
                        cols <- 1:8
                        edges[[1]][1]<-ths_sigs[2,1]
                        edges[[4]][length(edges[[4]])]<-ths_sigs[8,1]
                        
                }else if(corn==2){
                        rows <- 1:8
                        cols <- ncol(pic[[1]])+8+(1:8)
                        edges[[1]][length(edges[[1]])]<-ths_sigs[2,1]
                        edges[[2]][1]<-ths_sigs[4,1]
                }else if(corn==3){
                        rows <- nrow(pic[[1]])+8+(1:8)
                        cols <- ncol(pic[[1]])+8+(1:8)
                        edges[[2]][length(edges[[2]])]<-ths_sigs[4,1]
                        edges[[3]][1]<-ths_sigs[6,1]
                }else{
                        rows <- nrow(pic[[1]])+8+(1:8)
                        cols <- 1:8
                        edges[[3]][length(edges[[3]])]<-ths_sigs[6,1]
                        edges[[4]][1]<-ths_sigs[8,1]
                }
                new_pic[rows,cols] <- ths_grd[2:9,2:9]
        }
        return(list(new_pic,edges))
}

build_board <- function(tiles){
        sigs <- compile_sigs(tiles)
        layers <- find_layers(compile_sigs(tiles))
        pic <- init_pic(layers,tiles,sigs)
        for(i in (length(layers)-1):1){
                pic <- add_layer(pic,layers,i,tiles,sigs)
        }
        return(pic)
}
monster <-matrix(nrow=3,data =strsplit('                  # #    ##    ##    ### #  #  #  #  #  #   ','')[[1]],byrow = TRUE)

tmp_pic <- matrix(nrow=24,data =strsplit('.#.#..#.##...#.##..########....#.#....#..#......##.##.###.#.#..######...###.#####...#.#####.#..###.#....#.##.####...#.##...########.#....#####.#....#..#...##..#.#.###...####...#..#.....#......#..#.##..#..###.#.##....#.####..#.####.#.#.###..###.#.#...#.######.#..###.####....##..########.###..##.#...#...#.#.#.#.....#..#..#.#.##..###.###.#.#....#.##.#...###.##.###.#...#..#.##.######...#.#.###.##.##.#..#.##...####.###.#...###.#..#.#..#.#..#..#.#.#.####.####..####...#.#.#.###.###.#####..#####...###....###.##..#..#...#..####...#.#.###..##..##..####.##....###...##...#...#..###','')[[1]],byrow = TRUE)

check_monster <- function(img,monster){
        check <- img[monster=='#']
        if(all(check=='#'|check=='O')){
                img[monster=='#']<-'O'
                return(list(TRUE,img))
        }else{
                return(list(FALSE,img))
        }
}

seek_monst <- function(board,monster){
        count <- 0
        for(i in 1:(nrow(board)-nrow(monster)+1)){
                for(j in 1:(ncol(board)-ncol(monster)+1)){
                        res <- check_monster(board[i:(i+nrow(monster)-1),j:(j+ncol(monster)-1)],monster)
                        if(res[[1]]){
                                board[i:(i+nrow(monster)-1),j:(j+ncol(monster)-1)] <- res[[2]]
                                count <- count + 1
                        }
                }
        }
        if(count==0){
                return(NULL)
        }else{
                return(board)
        }
}

find_water <- function(board,monster){
        for(i in 1:8){
                res <- seek_monst(board,monster)
                if(!is.null(res)){
                        print(i)
                        return(res)
                }
                if(i == 4){
                        board <- transform_grid(board,matrix(nrow=8,ncol=2,data=0),'flip')[[1]]
                }else{
                        board <- transform_grid(board,matrix(nrow=8,ncol=2,data=0),'rotate')[[1]]
                }
        }
        return(NULL)
}

pic <- build_board(tiles)[[1]]
print(sum(find_water(pic,monster)=='#'))
