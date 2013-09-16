xwilcox.dist<- function(dist,design,groups='all',subgroups=FALSE, between=F){
  between.dist <- 
    function(dist,design,groups,subgroups=FALSE){                  # Get the   
      if(length(design)==1){design<-read.table(design)}
      if(length(design)>1){design<-design}
      if(subgroups==FALSE){
        if(ncol(design)==1){warning("design needs at least a name and group column with an optional subgroup column" )}
        if(ncol(design)==3){design<-design[,-3]}
        if(ncol(design)>3){warning("Too many columns design  should be in the form [names  groups  subgroups*] subgroups are optional")}
      }
      if(subgroups==TRUE){
        if(ncol(design)==1){warning("design needs atleast a name and group column with an optional subgroup column")}
        if(ncol(design)==2){warning("no subgroup column")}
        if(ncol(design)==3){design<-design[,-2]}
        if(ncol(design)>3){warning("Too many columns design should be in the form  [names  groups  subgroups*] subgroups are optional")}
      }
      if(length(groups)==1){                                       # if there is only one group 
        options(warn=(-1))                                         # Wait for warnings untill between.dist is returned
        if(groups=='all'){                                         # if the group file is all groups
          if(length(design)==1){dsn<-read.table(design)}           # if the design file is just the group names read the design file
          if(length(design)>1){dsn<-design}                        # if the design file contains both the group names and the point names set design to dsn
          groups<-as.vector(unique(dsn[,2]))                       # set groups to just the group names. 
          options(warn=(0))                                        # wait for warnings untill betwen.dist is returned.
        }
        
      }
      one.between<-function(dist,design,group1,group2){            #
        if(typeof(dist)=='character') { dist <- read.dist(dist) }# if the dist file is a character vector then setx ft dst to readdist(dist)                                                                                                                                                                       
        if(length(design)==1) { design<-read.design(design) }   # if the design file is just the groups the set it to design
        get.dists <- function(dist,design,group1,group2){  #
          group1 <<- group1                                     # group1 is globally group1
          group2 <<- group2                                     # group 2 is previous group 2
          get.grp1 <- function(d , g){                          #  get all names of a group. 
            c1 <- 1                                             # Set c1 for use in a loop
            grp1 <- c()                                         # Prepare grp1 for use in a loop
            if (nrow(d) >= 1) {
              for(i in 1:nrow(d)){                                # for every row in d 
                if(d[i,2]==g){                                   # if that row is that group 
                  grp1[c1] <- d[i,1]                             # the set row c1 = d[i,1]
                  c1 <- c1+1                                     # increase c1 by 1 ifor the next group
                }
              }
            }
            invisible(grp1)                                    # return grp1 wihout printing it.
          }
          get.grp2 <- function(d, g){                          # same as get.grp1 
            c2 <- 1                                            #  
            grp2 <- c()                                        #
            for(i in 1:nrow(d)){                               #
              if(d[i,2]==g){                                   #
                grp2[c2] <- d[i,1]                             #
                c2 <- c2+1                                     #
              }
            }
            invisible(grp2)                                    #
          }
          
          grp1 <- get.grp1(design,group1)                      # Get group 1
          grp2 <- get.grp2(design,group2)                      # Get group 2
          
          get.rows <- function(dist, grp){                     # given a dist and group get only the rows of that group
            rows <- dist[grp[1],]                              # rows becomes the row with grp1's first point name.
            
            if(length(grp)==2){
              rows <- rbind(rows,dist[grp[2],])
            }
            if(length(grp)>2){
              for(i in 2:length(grp)){                           #  put all of the other row's that ahave 
                rows <- rbind(rows,dist[grp[i],])                    #      
              }
            }
            invisible(rows)                                        #
          }
          rows <- get.rows(dist,grp1)                              # get the rows of group 1 
          
          get.rows <- function(dist, grp){                         # Similar to get rows of above but for the group2 columns 
            matrix <- dist[,grp[1]]                                # 
            if(length(grp)==2){
              matrix <- cbind(matrix,dist[,grp[2]])
            }
            if(length(grp)>2){
              
              for(i in 2:length(grp)){                               #
                matrix <- cbind(matrix,dist[,grp[i]])                #    
              }
            }
            invisible(matrix)                                      # returns a matrix
          }
          matrix <- get.rows(rows,grp2)    # get the columns of group 2
          vect <- as.vector(matrix)    #
          
          invisible(vect)  # reurn grp 2 as a matrix
        }
        dists <- get.dists(dist,design,group1,group2)   # use get.dists using the dist,design and the 2 groups
        return(dists) # return this1
      }  
      
      dist.vect <- c()  
      c <- 1
      dist.list <- vector(mode='list',length=((length(groups)^2-length(groups))/2))  
      list.names <- c() 
      for(i in 1:(length(groups)-1)){ 
        for(j in (i+1):length(groups)){  
          list.names[c] <- paste(groups[i], 'vs', groups[j]) 
          dist.list[[c]] <- one.between(dist,design,groups[i],groups[j]) 
          c <- c+1 
        }
      }
      
      names(dist.list) <-list.names
      invisible(dist.list)
      return(dist.list)
      
    }
  within.dist <-
    function(dist,design,groups, subgroups=FALSE){
      if(length(design)==1){design<-read.table(design)}
      if(length(design)>1){design<-design}
      if(subgroups==FALSE){
        if(ncol(design)==1){warning("design needs at least a name and group column with an optional subgroup column" )}
        if(ncol(design)==3){design<-design[,-3]}
        if(ncol(design)>3){warning("Too many columns design should be in the form [names  groups  subgroups*] subgroups are optional")}
      }
      if(subgroups==TRUE){
        if(ncol(design)==1){warning("design needs atleast a name and group column with an optional subgroup column")}
        if(ncol(design)==2){warning("no subgroup column")}
        if(ncol(design)==3){design<-design[,-2]}
        if(ncol(design)>3){warning("Too many columns design should be in the form  [names  groups  subgroups*] subgroups are optional")}
      }  
      
      if(length(groups)==1){
        options(warn=(-1))    
        if(groups=='all'){
          if(length(design)==1){dsn<-read.table(design)}
          if(length(design)>1){dsn<-design}
          groups<-as.vector(unique(dsn[,2]))
          options(warn=(0))    
        }
      }
      one.within<-function(dist,design,group){     
        if(typeof(dist)=='character') { dist<-read.dist(dist) }
        if(length(design)==1) { design<-read.design(design) }
        
        get.dists<-function(dist,design,group1,group2){
          get.grp1<-function(d , g){ 
            c1<-1
            grp1<-c()
            for(i in 1:nrow(d)){
              if(d[i,2]==g){
                grp1[c1]<-d[i,1]
                c1<-c1+1
              }
            }
            invisible(grp1)
          }
          get.grp2<-function(d, g){ 
            c2<-1
            grp2<-c()
            for(i in 1:nrow(d)){
              if(d[i,2]==g){
                grp2[c2]<-d[i,1]
                c2<-c2+1
              }
            }
            invisible(grp2)
          }
          
          grp1<-get.grp1(design,group1)
          grp2<-get.grp2(design,group2)
          
          get.rows<-function(dist, grp){
            rows<-dist[grp[1],]
            if(length(grp)==2){
              rows <- rbind(rows,dist[grp[2],])
            }
            if(length(grp)>2){
              for(i in 2:length(grp)){
                rows<-rbind(rows,dist[grp[i],])      
              }
            }
            invisible(rows)
          }
          rows<-get.rows(dist,grp1)
          
          get.rows<-function(dist, grp){
            matrix<-dist[,grp[1]]
            if(length(grp)==2){
              matrix <- cbind(matrix,dist[,grp[2]])
            }
            if(length(grp)>2){
              for(i in 2:length(grp)){
                matrix<-cbind(matrix,dist[,grp[i]])      
              }
            }
            invisible(matrix)
          }
          matrix<-get.rows(rows,grp2)
          vect<-as.vector(matrix)
          invisible(vect)
        }
        dists<-get.dists(dist,design,group,group)
        #print(dists)
        rm.duplicates<-function(dists){
          if(length(dists)==0){
            half.dists<- c(0,0,0,0,0,0,0,0,0)
          }
          if (length(dists)==1){
            half.dists <- 0.000000
          }
          if(length(dists)>1){
            square<-matrix(dists,nrow=sqrt(length(dists)))
            half.dists<-c()
            half.dists[1]<-square[2,1]
            if(ncol(square)==3){
              keepers<- square[3,1:2]
              half.dists[(length(half.dists)+1):(length(half.dists)+length(keepers))] <- keepers
            }
            if(ncol(square)>3){
              for(i in 3:ncol(square)){
                keepers<-square[i,1:(i-1)] 
                half.dists[(length(half.dists)+1):(length(half.dists)+length(keepers))] <- keepers
              }
            }
          }
          invisible(half.dists)      
        }
        lt.dist<-rm.duplicates(dists)
       
        invisible(lt.dist)
        
      }
      dist.vect<-c()
      c<-1
      dist.list<-vector(mode='list',length=length(groups))
      for(i in groups){
        dist.list[[c]]<-one.within(dist,design,i)
        c<-c+1
      }
      
      names(dist.list)<-groups
      invisible(dist.list)
    }
  if(between == T){
    distances <- between.dist(dist,design,groups,subgroups=subgroups)
  
    for(i in 1:(length(distances)-1)){ 
      for(j in (i+1):length(distances)){
        names <-names(distances)
        printme <- paste(sprintf("wilcox of %s and %s", names[i], names[j]), sep='')
        print(printme)
        alpha  <- distances[i]
        alpha  <- as.data.frame(alpha)
        X <- c()
        X <- c(alpha[,1:length(alpha)])
        X <- as.double(X)
        beta   <- distances[j]
        beta   <- as.data.frame(beta)
        Y  <- c()
        Y  <- c(beta[,1:length(beta)])
        Y  <- as.double(Y)
        print(wilcox.test(X,Y,conf.int=T))
      }
    }  
  }
  if(between==F){
    distances <- within.dist(dist,design,groups,subgroups=subgroups)
    for(i in 1:(length(distances)-1)){
      for(j in (i+1):length(distances)){
        names <-names(distances)
        printme <- paste(sprintf("wilcox of %s(x), and %s(y).", names[i], names[j]), sep='')
        print(printme)
        alpha  <- distances[i]
        alpha  <- as.data.frame(alpha)
        X <- c()
        X <- c(alpha[,1:length(alpha)])
        X <- as.double(X)
        beta   <- distances[j]
        beta   <- as.data.frame(beta)
        Y  <- c()
        Y  <- c(beta[,1:length(beta)])
        Y  <- as.double(Y)
        print(wilcox.test(X,Y,conf.int=T))
      }
    }    
  }
}