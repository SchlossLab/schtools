plotPCOA3d         <- function(file,design=FALSE,ax1=1, ax2=2, ax3=3, Lab1="Axis 1",Lab2="Axis 2",
                             Lab3="Axis 3", Pch = 19, Main = NULL, Sub=NULL,
                             Scale.y=1, Angle=40,
                             Axis=TRUE, Tick.marks=TRUE, Label.tick.marks=TRUE,
                             X.ticklabs=NULL, Y.ticklabs=NULL, Z.ticklabs=NULL,
                             Y.margin.add=0, Grid=TRUE, Box=TRUE, Lab=par("lab"),
                             Lab.z=mean(Lab[1:2]), Type="p", LX="topleft", LY= NULL){
  require(scatterplot3d)
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  if (substrRight(file,5) != ".axes") {
    print("warning file not .axes")
  }
  data           <- read.table(file,header = T)
  names          <- data[,1]
  axis           <- data[,-1]
  axi            <- axis[,c(ax1,ax2,ax3)]
  colnames(axi) <- c("axis1","axis2","axis3")
  
  if (design==FALSE){
    scatterplot3d(x = axi$"axis1", y = axi$"axis2", z = axi$"axis3",xlab = Lab1, ylab = Lab2, zlab = Lab3,
                  pch=Pch, main=Main, sub=Sub, scale.y=Scale.y,
                  angle=Angle, axis=Axis, tick.marks=Tick.marks,label.tick.marks=Label.tick.marks,
                  x.ticklabs=X.ticklabs, y.ticklabs=Y.ticklabs, z.ticklabs=Z.ticklabs, y.margin.add=Y.margin.add,
                  grid=Grid, box=Box, lab=Lab, lab.z=Lab.z, type=Type)
  }
  else{
    if (substrRight(design,7) != ".design") {
      warning("Warning message: design file is not .design")
    }
    designtable    <- suppressWarnings(read.table(design, header=F))
    tDesign        <- t(designtable)
    taxi           <- cbind(tDesign[,2],axi)
    names(taxi)[1] <- "partition" 
    
    scatterplot3d(x = taxi$"axis1", y = taxi$"axis2", z = taxi$"axis3", xlab = Lab1, ylab = Lab2, zlab = Lab3,
                  color = as.integer(factor(taxi$"partition")), pch = Pch, main=Main, sub=Sub, scale.y=Scale.y,
                  angle=Angle, axis=Axis, tick.marks=Tick.marks,label.tick.marks=Label.tick.marks,
                  x.ticklabs=X.ticklabs, y.ticklabs=Y.ticklabs, z.ticklabs=Z.ticklabs, y.margin.add=Y.margin.add,
                  grid=Grid, box=Box, lab=Lab, lab.z=Lab.z, type=Type)
    legend(x=LX,y=LY, legend = c(levels(taxi$partition)), col = as.integer(factor(levels(taxi$"partition"))), pch = Pch)
  }
}