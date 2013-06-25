\name{plotPCOA3d}
\title{
Plot PCOA in 3d
}
\description{
Plot the pcoa.axes output from mothur. Can accept a design file for groups. 
}
\usage{
plotPCOA3d(file,design=FALSE,ax1=1, ax2=2, ax3=3, Lab1="Axis 1",Lab2="Axis 2",
Lab3="Axis 3", Pch = 19, Main = NULL, Sub=NULL, Scale.y=1, Angle=40,
Axis=TRUE, Tick.marks=TRUE, Label.tick.marks=TRUE, X.ticklabs=NULL,
Y.ticklabs=NULL, Z.ticklabs=NULL, Y.margin.add=0, Grid=TRUE, Box=TRUE,
Lab=par("lab"), Lab.z=mean(Lab[1:2]), Type="p", LX="topleft", LY= NULL)
}

\arguments{
  \item{file}{
  The .pcoa.axes file from mothur's pcoa function.
  }
  \item{design}{
  The .design file for the groups you which to color the points by. defaults to no design file.  
  }
  \item{ax1, ax2, ax3}{
  The axes you wish to plot. Defaults to the 3 most relevant according to mothur.
  }
  \item{Lab1, Lab2, Lab3}{
  titles for the 1st/x, 2nd/y and 3rd/z axis.
  }
  \item{Pch}{
  plotting "character", i.e. symbol to use. Defaults to 19 (solid circles)
  }
  \item{Main}{
  an overall title for the plot.
  }
  \item{Sub}{
  sub-title.
  }
  \item{Scale.y}{
   scale of y axis related to x- and z axis.
  }
  \item{Angle}{
   angle between x and y axis (Attention: result depends on scaling).
  }
  \item{Axis}{
   a logical value indicating whether axes should be drawn on the plot.
  }
  \item{Tick.marks}{
   a logical value indicating whether tick marks should be drawn on the plot (only if axis = TRUE).
  }
  \item{Label.tick.marks}{
   a logical value indicating whether tick marks should be labeled on the plot (only if axis = TRUE and tick.marks = TRUE).
  }
  \item{X.ticklabs, Y.ticklabs, Z.ticklabs}{
   vector of tick mark labels.
  }
  \item{Y.margin.add}{
   add additional space between tick mark labels and axis label of the y axis.
  }
  \item{Grid}{
  a logical value indicating whether a grid should be drawn on the plot.
  }
  \item{Box}{
  a logical value indicating whether a box should be drawn around the plot.
  }  
  \item{Lab}{
  a numerical vector of the form c(x, y, len). The values of x and y give the (approximate) number of tickmarks on the x and y axes.
  }  
  \item{Lab.z}{
  the same as lab, but for z axis.
  }  
  \item{Type}{
  character indicating the type of plot: "p" for points, "l" for lines, "h" for vertical lines to x-y-plane, etc.
  } 
  \item{LX, LY}{
  the x and y co-ordinates to be used to position the legend. They can be specified by keyword or in any way which is accepted by xy.coords.
  }
}
\details{

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
The mothuR page on mothur's wiki: \code{\link{http://www.mothur.org/wiki/mothuR}}

}
\author{
Andrew Hanna: ahanna12345@gmail.com
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
plotPCOA3d(exeamplefile.pcoa.axes,examplefile.design)
plots the first 3 axis with colors dividing partitions. 

plotPCOA3d(examplefile2.pcoa.axes,examplefile2.design,ax2=4, ax3=5, 
           Lab2=="axis4", Lab3= "axis5",  Main="Title", Box=FALSE)
Plots axis 1,4,5 and changes the lables to match. Adds Title "Title" and removes the box sorrounding the points.
}
\keyword{pcoa}
\keyword{pcoa3d}
\keyword{PCoA3d}
\keyword{PCoA}