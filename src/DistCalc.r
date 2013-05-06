#Imports a phylip-formatted distance matrix to be analyzed
read.dist<-function(file){
    pre.dist<-read.table(file, skip=1)
    dist<-pre.dist[2:ncol(pre.dist)]
    colnames(dist) <- t(pre.dist[1])
    rownames(dist) <- t(pre.dist[1])
    invisible(dist)
}


#Imports the design file that separates samples into groups
read.design<-function(file){
    design<-read.table(file)
    design<-matrix(unlist(design),ncol=2)
    colnames(design) <- c('sample','group')
    invisible(design)
}

#Returns min, max, mean, median, and sd of distance between two groups
between.dist<-function(dist,design,groups){

    if(length(groups)==1){
        options(warn=(-1))    
        if(groups=='all'){
            if(length(design)==1){dsn<-read.table(design)}
            if(length(design)>1){dsn<-design}
            groups<-as.vector(unique(dsn[,2]))
            options(warn=(0))    
        }
    }
    one.between<-function(dist,design,group1,group2){
        if(typeof(dist)=='character') { dist<-read.dist(dist) }
        if(length(design)==1) { design<-read.design(design) }
        get.dists<-function(dist,Detailsdesign,group1,group2){
            group1<<-group1
            group2<<-group2
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
                for(i in 2:length(grp)){
                    rows<-rbind(rows,dist[grp[i],])      
                }
                invisible(rows)
            }
            rows<-get.rows(dist,grp1)
            
            get.rows<-function(dist, grp){
                matrix<-dist[,grp[1]]
                for(i in 2:length(grp)){
                    matrix<-cbind(matrix,dist[,grp[i]])      
                }
                invisible(matrix)
            }
            matrix<-get.rows(rows,grp2)
            vect<-as.vector(matrix)
            invisible(vect)
        }
        dists<-get.dists(dist,design,group1,group2) 
        cat(sprintf("Stats for distances between %s and %s", group1, group2), "\n", sprintf('Minimum: %f', min(dists)), "\n", sprintf('Maximum: %f', max(dists)), "\n", sprintf('Median: %f', median(dists)), "\n",sprintf('Mean: %f', mean(dists)), "\n", sprintf('Std. Dev.: %f', sd(dists)), "\n", "\n", sep='')
        return(dists)
    }  
     
    dist.vect<-c()
    c<-1
    dist.list<-vector(mode='list',length=((length(groups)^2-length(groups))/2))
    list.names<-c()
    for(i in 1:(length(groups)-1)){
        for(j in (i+1):length(groups)){
            list.names[c]<-paste(groups[i], 'vs', groups[j])
            dist.list[[c]]<-one.between(dist,design,groups[i],groups[j])
            c<-c+1
        }
    }
   
    names(dist.list)<-list.names
    invisible(dist.list)
}
#Returns min, max, mean, median, and sd of distance within a group
within.dist<-function(dist,design,groups){
    
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
                for(i in 2:length(grp)){
                    rows<-rbind(rows,dist[grp[i],])      
                }
                invisible(rows)
            }
            rows<-get.rows(dist,grp1)
            
            get.rows<-function(dist, grp){
                matrix<-dist[,grp[1]]
                for(i in 2:length(grp)){
                    matrix<-cbind(matrix,dist[,grp[i]])      
                }
                invisible(matrix)
            }
            matrix<-get.rows(rows,grp2)
            vect<-as.vector(matrix)
            invisible(vect)
        }
        dists<-get.dists(dist,design,group,group)  
        rm.duplicates<-function(dists){
            square<-matrix(dists,nrow=sqrt(length(dists)))
            half.dists<-c()
            half.dists[1]<-square[2,1]
            for(i in 3:ncol(square)){
                keepers<-square[i,1:(i-1)] 
                half.dists[(length(half.dists)+1):(length(half.dists)+length(keepers))] <- keepers
            }
            invisible(half.dists)
        }
        lt.dist<-rm.duplicates(dists)
        cat(sprintf("Stats for distances within %s", group), "\n", sprintf('Minimum: %f', min(lt.dist)), "\n", sprintf('Maximum: %f', max(lt.dist)), "\n", sprintf('Median: %f', median(lt.dist)), "\n",sprintf('Mean: %f', mean(lt.dist)), "\n", sprintf('Std. Dev.: %f', sd(lt.dist)), "\n", "\n", sep='')
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


#Makes a barplot using the output from either between.dist or within.dist
plot.dists<-function(list, space=NULL, col=c(), ylim=c(0,1), ylab='Average Distance', xlab='', main=c()){
    avgs<-c()
    for(i in 1:length(list)){
        avgs[i]<-mean(list[[i]])
    }
    
    if(length(col)==0){col<-rgb(runif(length(list)),runif(length(list)),runif(length(list)))}
    xcor<-barplot(avgs,names.arg=names(list), space=space, ylim=ylim, col=col, xlab=xlab, ylab=ylab, main=main)
    
    for(i in 1:length(list)){
        arrows(x0=xcor[i], x1=xcor[i], y0=mean(list[[i]]), y1=mean(list[[i]])+sd(list[[i]]), angle=90)
    }
}

gf.design <- read.design('germfree.design')
gf.sq.dist <- read.dist('germfree.sq.dist')
