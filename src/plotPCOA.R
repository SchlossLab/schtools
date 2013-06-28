plotPCOA<- function(axes,design=FALSE, ax1=1, ax2=2, Lab1="axis1", Lab2= "axis2",  psize=I(4.5)){
  require(ggplot2) 
  axi             <- read.table(axes,header = T)
  if (design==FALSE){
    qplot(data= axi,x= axis1, y=axis2, xlab=Lab1, ylab=Lab2,..., size = I(psize))
  }
  else {
    design         <- suppressWarnings(read.table(design, header=F))
    tDesign        <- t(design)
    taxi           <- cbind(tDesign[,2],axi)
    names(taxi)[1] <- "partition" 
    groups         <- taxi[1]
      qplot(data=taxi, x=axis1, y=axis2, color=partition, size = I(psize))
   
  }
}
#plotPCOA<- function(axes,design=FALSE, ax1=1, ax2=2, Lab1="axis1", Lab2= "axis2",
#colour=partition,shape=I(19),geom=I("point"), psize=I(4.5),main="PCOA"){
#  require(ggplot2) 
#  axi             <- read.table(axes,header = T)
#  if (design==FALSE){
#    qplot(data= axi,x= axis1, y=axis2, xlab=Lab1, ylab=Lab2,..., size = psize)
#  }
#  else {
#    design         <- suppressWarnings(read.table(design, header=F))
#    tDesign        <- t(design)
#    taxi           <- cbind(tDesign[,2],axi)
#    names(taxi)[1] <- "partition" 
#    groups         <- taxi[1]
#    theme_set(theme_gray(base_size = 16))
    
 #   qplot(data=taxi, x=axis1, y=axis2, color=colour, size = psize)
     
  

