RarePlot <- function(x,y,ylab= "Number of Different OTUs",xlab= "Number of Tags Sampled",pch= NA, xlim = NULL, ylim = NULL, error=FALSE) #Same Defaults as matplot
{
  library(reshape2)
  require(ggplot2)#install.packages("ggplot2")
  Data <- read.table(x, header=T)
  Length <- length(Data[1,])
  if (error == FALSE)
  {
    Steps <- seq(from=2,to=Length-2,by=3)
    TrueDat <- Data[,c(1,Steps)]
    Names <- names(TrueDat)
    substr(names(TrueDat), 1, 6) <- ""
    substr(names(TrueDat), 1, 6) <- ""
    substr(names(TrueDat), 1, 6) <- ""
    substr(names(TrueDat), 1, 6) <- ""
    substr(names(TrueDat), 1, 6) <- ""
    substr(names(TrueDat), 1, 6) <- ""
    names(TrueDat)[1] <- "Number"
    Number <- TrueDat[1]
    Columns <- TrueDat[-1]
    matplot(Number,Columns[,y],ylab= ylab,xlab = xlab, pch=pch,col=1:15, xlim = xlim, ylim = ylim)
    legend("topleft",legend=names(Columns[,y]), col =1:15,pch=19)
    matlines(Number,Columns[,y], col=1:15)
  }
  else 
  {
    PureSteps <- seq(from=2,to=Length-2,by=3)
    LowSteps <- seq(from=3,to=Length-1,by=3)
    HighSteps <- seq(from=4,to=Length,by=3)
    PureDat <- Data[,c(1,PureSteps)]
    LowDat <- Data[,c(1,LowSteps)]
    HighDat <- Data[,c(1,HighSteps)]
    PureNames <- names(PureDat)
    substr(names(PureDat), 1, 6) <- ""
    substr(names(PureDat), 1, 6) <- ""
    substr(names(PureDat), 1, 6) <- ""
    substr(names(PureDat), 1, 6) <- ""
    substr(names(PureDat), 1, 6) <- ""
    substr(names(PureDat), 1, 6) <- ""
    names(PureDat)[1] <- "Number"
    PureColumns <- PureDat[-1]
    matplot(Number,PureColumns[,y],ylab= ylab,xlab = xlab, pch=pch,col=1:15, xlim = xlim, ylim = ylim)
    legend("topleft",legend=names(PureColumns[,y]), col =1:15,pch=19)
    matlines(Number,PureColumns[,y], col=1:15)
    LowColumns<- LowDat[-1]
    matlines(Number,LowColumns[,y], col=1:15, lwd=.2)
    HighColumns<- HighDat[-1]
    matlines(Number,HighColumns[,y], col=1:15, lwd=.2)
  }
}