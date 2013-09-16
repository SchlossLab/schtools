read.dist <-
function(file){
    pre.dist<-read.table(file, skip=1)
    dist<-pre.dist[2:ncol(pre.dist)]
    colnames(dist) <- t(pre.dist[1])
    rownames(dist) <- t(pre.dist[1])
    invisible(dist)
}
