<<<<<<< HEAD
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
=======
rarePlot <- function(file, groups, ylab = "Number of Different OTUs", xlab = "Number of Tags Sampled",
                     pch = NA, xlim = NULL, ylim = NULL, error = FALSE){ 
  library(reshape2)
  require(ggplot2)                  # install.packages("ggplot2")
  Data                           <- read.table(file, header = T ) # put Data in Dataframe
  Length                         <- length(Data[1, ]) 
  if (error == FALSE) {  # If we don't want errorbars  
    Steps                        <- seq(from = 2, to = Length - 2, by = 3)  # Select only the value not the Lower or Upper Bound
    TrueDat                      <- Data[, c(1, Steps)] 
    substr(names(TrueDat), 1, 6) <- "" # This deletes the first character of the groupname
    substr(names(TrueDat), 1, 6) <- ""  # Repeat 6 times so you only have the name
>>>>>>> Updated Stacked Bar Chart and RarePlot w/ Documentation
    substr(names(TrueDat), 1, 6) <- ""
    substr(names(TrueDat), 1, 6) <- ""
    substr(names(TrueDat), 1, 6) <- ""
    substr(names(TrueDat), 1, 6) <- ""
<<<<<<< HEAD
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
=======
    names(TrueDat)[1]            <- "Number"
    Number                       <- TrueDat[1]
    Columns                      <- TrueDat[-1]
    matplot(Number, Columns[, groups], ylab = ylab, xlab = xlab, pch = pch, col = 1:15, xlim = xlim, ylim = ylim) #Plot the groups you want
    legend("topleft", legend = names(Columns[, groups]), col = 1:15, pch = 19)  # Make a legend
    matlines(Number, Columns[, groups], col = 1:15)  # Make lines conecting the points
  }
  else {  # If we do want errorbars
    PureSteps                    <- seq(from = 2, to = Length - 2, by = 3)
    LowSteps                     <- seq(from = 3, to = Length - 1, by = 3)  # Select only the lower bounds
    HighSteps                    <- seq(from = 4, to = Length, by = 3)  # Select only the upper bounds
    PureDat                      <- Data[ , c(1, PureSteps)]
    LowDat                       <- Data[ , c(1, LowSteps)]
    HighDat                      <- Data[ , c(1, HighSteps)]
>>>>>>> Updated Stacked Bar Chart and RarePlot w/ Documentation
    substr(names(PureDat), 1, 6) <- ""
    substr(names(PureDat), 1, 6) <- ""
    substr(names(PureDat), 1, 6) <- ""
    substr(names(PureDat), 1, 6) <- ""
    substr(names(PureDat), 1, 6) <- ""
    substr(names(PureDat), 1, 6) <- ""
<<<<<<< HEAD
    names(PureDat)[1] <- "Number"
    PureNumber <- PureDat[1]
    PureColumns <- PureDat[-1]
    LowNames <- names(LowDat)
    substr(names(LowDat), 1, 6) <- ""
    substr(names(LowDat), 1, 6) <- ""
    substr(names(LowDat), 1, 6) <- ""
    substr(names(LowDat), 1, 6) <- ""
    names(LowDat)[1] <- "Number"
    LowNumber <- LowDat[1]
    LowColumns <- LowDat[-1]
    HighNames <- names(LowDat)
=======
    names(PureDat)[1]            <- "Number"
    PureNumber                   <- PureDat[1]
    PureColumns                  <- PureDat[-1]
    substr(names(LowDat), 1, 6)  <- ""
    substr(names(LowDat), 1, 6)  <- ""
    substr(names(LowDat), 1, 6)  <- ""
    substr(names(LowDat), 1, 6)  <- "" 
    names(LowDat)[1]             <- "Number"
    LowNumber                    <- LowDat[1]
    LowColumns                   <- LowDat[-1]
>>>>>>> Updated Stacked Bar Chart and RarePlot w/ Documentation
    substr(names(HighDat), 1, 6) <- ""
    substr(names(HighDat), 1, 6) <- ""
    substr(names(HighDat), 1, 6) <- ""
    substr(names(HighDat), 1, 6) <- ""
<<<<<<< HEAD
    names(HighDat)[1] <- "Number"
    HighNumber <- HighDat[1]
    HighColumns <- HighDat[-1]
    matplot(PureNumber,PureColumns[,y],ylab= ylab,xlab = xlab, pch=pch,col=1:15, xlim = xlim, ylim = ylim)
    legend("topleft",legend=names(PureColumns[,y]), col =1:15,pch=19)
    matlines(PureNumber,PureColumns[,y], col=1:15)
    LowColumns<- LowDat[-1]
    matlines(LowNumber,LowColumns[,y], col=1:15, lwd=.2)
    HighColumns<- HighDat[-1]
    matlines(HighNumber,HighColumns[,y], col=1:15, lwd=.2)
  }
}
=======
    names(HighDat)[1]            <- "Number"
    HighNumber                   <- HighDat[1]
    HighColumns                  <- HighDat[-1]
    matplot(PureNumber, PureColumns[, y], ylab = ylab, xlab = xlab, pch = pch,
            col = 1:15, xlim = xlim, ylim = ylim)  # Plot center points
    legend("topleft", legend = names(PureColumns[ , y]), col = 1:15, pch = 19)  # Plot Legend
    matlines(PureNumber, PureColumns[, y], col = 1:15)  # Plot center Line
    LowColumns                   <- LowDat[-1]
    matlines(LowNumber, LowColumns[, y], col = 1:15, lwd = .2)  # Plot faint Lowbound line
    HighColumns                  <- HighDat[-1]
    matlines(HighNumber, HighColumns[, y], col = 1:15, lwd = .2)  # Plot faint Highbound line
  }
}     
>>>>>>> Updated Stacked Bar Chart and RarePlot w/ Documentation
