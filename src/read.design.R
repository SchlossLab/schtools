read.design <-
function(file){
    design<-read.table(file)
    design<-matrix(unlist(design),ncol=2)
    colnames(design) <- c('sample','group')
    invisible(design)
}
