plot.dists <-
function(list, space=NULL, col=c(), ylim=c(0,1), ylab='Average Distance', xlab='', main=c()){
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
