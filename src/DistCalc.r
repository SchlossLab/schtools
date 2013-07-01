#Imports a phylip-formatted distance matrix to be analyzed
read.dist<-function(file){ 
    pre.dist<-read.table(file, skip=1) # Skip first row but read the phylip table into a variable
    dist<-pre.dist[2:ncol(pre.dist)]   # make a new variable which doesn't include the point names
    colnames(dist) <- t(pre.dist[1])   # set column names to the corrosponding point name
    rownames(dist) <- t(pre.dist[1])   # set the row names to the correct point name
    invisible(dist)                    #return dist for later use
}


#Imports the design file that separates samples into groups
read.design<-function(file){
    design<-read.table(file)                #read the file
    design<-matrix(unlist(design),ncol=2)   #unlist it.
    colnames(design) <- c('sample','group') # name the columns
    invisible(design)                       #return design for later use
}

#Returns min, max, mean, median, and sd of distance between two groups
between.dist<-function(dist,design,groups){
    if(length(groups)==1){                                 # if there is only one group 
        options(warn=(-1))                                 # Wait for warnings untill between.dist is returned
        if(groups=='all'){                                 # if the group file is all groups
            if(length(design)==1){dsn<-read.table(design)} # if the design file is just the group names read the design file
            if(length(design)>1){dsn<-design}              # if the design file contains both the group names and the point names set design to dsn
            groups<-as.vector(unique(dsn[,2]))             # set groups to just the group names. 
            options(warn=(0))                              # wait for warnings untill betwen.dist is returned.
        }
    }
    one.between<-function(dist,design,group1,group2){
        if(typeof(dist)=='character') { dist<-read.dist(dist) }  # if the dist file is a character vector then setx ft dst to readdist(dist)
        if(length(design)==1) { design<-read.design(design) }    # if the design file is just the groups the set it to design
        get.dists<-function(dist,Detailsdesign,group1,group2){
            group1<<-group1    # group 1 is previous group 1
            group2<<-group2   # group 2 is previous group 2
            get.grp1<-function(d , g){ # given a design file and a group get all of the point names  within the group. (but not distances)
                c1<-1  # Set c1 for use in a loop
                grp1<-c()  # Prepare grp1 for use in a loop
                for(i in 1:nrow(d)){  # for every row in d that is of a group put in in grp1 of row c1 and make c1 move up 1
                    if(d[i,2]==g){ # if that row is that group 
                        grp1[c1]<-d[i,1]  # the set row c1 = d[i,1]
                        c1<-c1+1  # increase c1 by 1 ifor the next group
                    }
                }
                invisible(grp1)  # return grp1 wihout printing it.
            }
            get.grp2<-function(d, g){  # same as get.grp1 
                c2<-1  #  
                grp2<-c()  #
                for(i in 1:nrow(d)){  #
                    if(d[i,2]==g){  #
                        grp2[c2]<-d[i,1]  #
                        c2<-c2+1  #
                    }
                }
                invisible(grp2)  #
            }
            
            grp1<-get.grp1(design,group1)  # Get group 1
            grp2<-get.grp2(design,group2)  # Get group 2
            
            get.rows<-function(dist, grp){  # given a dist and group get only the rows of that group
                rows<-dist[grp[1],]  # rows becomes the row with grp1's first point name. 
                for(i in 2:length(grp)){  #  put all of the other row's that ahave 
                    rows<-rbind(rows,dist[grp[i],])  #      
                }
                invisible(rows)  #
            }
            rows<-get.rows(dist,grp1)  # get the rows of group 1 
            
            get.rows<-function(dist, grp){  # Similar to get rows of above but for the group2 columns 
                matrix<-dist[,grp[1]]  #  
                for(i in 2:length(grp)){  #
                    matrix<-cbind(matrix,dist[,grp[i]])  #    
                }
                invisible(matrix)  # returns a matrix
            }
            matrix<-get.rows(rows,grp2)  # get the columns of group 2
            vect<-as.vector(matrix)  #
            invisible(vect)  # reurn grp 2 as a matrix
        }
        dists<-get.dists(dist,design,group1,group2)   # use get.dists using the dist,design and the 2 groups
        cat(sprintf("Stats for distances between %s and %s", group1, group2), "\n", # Stats for distances between grp1 and grp 2
            sprintf('Minimum: %f', min(dists)), "\n", sprintf('Maximum: %f', max(dists)),  # minimum: (mindists)
            "\n", sprintf('Median: %f', median(dists)), "\n",sprintf('Mean: %f', mean(dists)),  # maximum: (maxdists) ; Mean: (mean(dists))
            "\n", sprintf('Std. Dev.: %f', sd(dists)), "\n", "\n", sep='') # Std.Dev.: (sd(dists))
        return(dists) # return this
    }  
     
    dist.vect<-c() # 
    c<-1 # start c for the loop
    dist.list<-vector(mode='list',length=((length(groups)^2-length(groups))/2)) #  length(groups=lg), length= lg^2-lg/2  
    list.names<-c() # prep list.names for the loop
    for(i in 1:(length(groups)-1)){ # 
        for(j in (i+1):length(groups)){ #for every j in 2 through end of groups 
            list.names[c]<-paste(groups[i], 'vs', groups[j]) # list of names i vs j
            dist.list[[c]]<-one.between(dist,design,groups[i],groups[j]) # get the in between data of i and j
            c<-c+1 #loop through it
        }
    }
   
    names(dist.list)<-list.names
    invisible(dist.list)
}
#Returns min, max, mean, median, and sd of distance within a group
within.dist<-function(dist,design,groups){ # 
    
    if(length(groups)==1){ # if only one group
        options(warn=(-1))   #  wait till end to show warnings
        if(groups=='all'){ # if groups are all
            if(length(design)==1){dsn<-read.table(design)} # if the design file is just 
            if(length(design)>1){dsn<-design} # if length is greater thn one then dsn isthe design file.
            groups<-as.vector(unique(dsn[,2])) # set the diffrent group names into a file.
            options(warn=(0))    
        }
    }
    one.within<-function(dist,design,group){     
        if(typeof(dist)=='character') { dist<-read.dist(dist) }
        if(length(design)==1) { design<-read.design(design) }
        
        get.dists<-function(dist,design,group1,group2){
            get.grp1<-function(d , g){   # given a design and a group for every row with group=g  get all in one group toghether
                c1<-1                    # prep the loop
                grp1<-c()                # prep grp1 for the loop
                for(i in 1:nrow(d)){     # for every row in d
                    if(d[i,2]==g){       # if the 2nd column value is g
                        grp1[c1]<-d[i,1] # add it to the group 1 list
                        c1<-c1+1         # move the next slot in the group 1 list down 1
                    }
                }
                invisible(grp1)
            }
            get.grp2<-function(d, g){    # similar to above but a 2nd group
                c2<-1                    # Prep c=2
                grp2<-c()                #  
                for(i in 1:nrow(d)){     # 
                    if(d[i,2]==g){       # 
                        grp2[c2]<-d[i,1] # 
                        c2<-c2+1         # 
                    }
                }
                invisible(grp2)#
            }
            
            grp1<-get.grp1(design,group1) # get group 1#
            grp2<-get.grp2(design,group2) # get group 2
            
            get.rows<-function(dist, grp){ # Function
                rows<-dist[grp[1],]        # every row with a name in group1
                for(i in 2:length(grp)){   # for every row with a name in group 1 
                    rows<-rbind(rows,dist[grp[i],]) #   
                }
                invisible(rows)
            }
            rows<-get.rows(dist,grp1)
            
            get.rows<-function(dist, grp){ # 
                matrix<-dist[,grp[1]] # all of the names of grp 1
                for(i in 2:length(grp)){# for every i in the length of grp
                    matrix<-cbind(matrix,dist[,grp[i]])      # columnbind (names of grp1 with every column of grp)
                }
                invisible(matrix) #
            }
            matrix<-get.rows(rows,grp2) # the 2 grps compared to each ther
            vect<-as.vector(matrix) #
            invisible(vect) #
        }
        dists<-get.dists(dist,design,group,group)  #
        rm.duplicates<-function(dists){#
            square<-matrix(dists,nrow=sqrt(length(dists)))#
            half.dists<-c()#
            half.dists[1]<-square[2,1]#
            for(i in 3:ncol(square)){#
                keepers<-square[i,1:(i-1)] #
                half.dists[(length(half.dists)+1):(length(half.dists)+length(keepers))] <- keepers#
            }#
            invisible(half.dists)#
        }
        lt.dist<-rm.duplicates(dists)#
        cat(sprintf("Stats for distances within %s", group), "\n", sprintf('Minimum: %f', min(lt.dist)),#  add all of these together
            "\n", sprintf('Maximum: %f', max(lt.dist)), "\n", sprintf('Median: %f', median(lt.dist)),#
            "\n",sprintf('Mean: %f', mean(lt.dist)), "\n", sprintf('Std. Dev.: %f', sd(lt.dist)),#
            "\n", sprintf('Std. Error.:%f', sd(lt.dist)/(sqrt(nrow(design)-1))), "\n", "\n", sep='')#
        invisible(lt.dist)#
        print(lt.dist)
    }
    dist.vect<-c()# 
    c<-1# start the loop
    dist.list<-vector(mode='list',length=length(groups)) # 
    for(i in groups){ #
        dist.list[[c]]<-one.within(dist,design,i) #
        c<-c+1 #
    }
    
    names(dist.list)<-groups#
    invisible(dist.list)#
}


#Makes a barplot using the output from either between.dist or within.dist
plot.dists<-function(list, space=NULL, col=c(), ylim=c(0,1), ylab='Average Distance', xlab='', main=c()){#
    avgs<-c()#
    for(i in 1:length(list)){#
        avgs[i]<-mean(list[[i]])#
    }
    
    if(length(col)==0){col<-rgb(runif(length(list)),runif(length(list)),runif(length(list)))}#
    xcor<-barplot(avgs,names.arg=names(list), space=space, ylim=ylim, col=col, xlab=xlab, ylab=ylab, main=main)#
    
    for(i in 1:length(list)){#
        arrows(x0=xcor[i], x1=xcor[i], y0=mean(list[[i]]), y1=mean(list[[i]])+sd(list[[i]]), angle=90)#
    }
}

#gf.design <- read.design('germfree.design')
#gf.sq.dist <- read.dist('germfree.sq.dist')
