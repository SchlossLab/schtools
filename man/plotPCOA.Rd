\name{plotPCOA}
\alias{plotPCOA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Mothur's PCoA Data with an optional design file for groups in 2d.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotPCOA(axes,design=FALSE, ax1=1, ax2=2, Lab1="axis1", Lab2= "axis2", psize=I(4.5))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{axes}{
  the .pcoa.axes file from mothur's pcoa function.
  }
  \item{design}{
  the .design file for the groups you which to color the points by. defaults to no design file.  
  }
  \item{ax1, ax2}{
  the axes you wish to plot. Defaults to the first 2.
  }
  \item{Lab1, Lab2}{
  titles for the 1st/x and 2nd/y axes.
  }
  \item{psize}{
  Size of the points plotted.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
Andrew Hanna (ahanna12345gmail.com)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
plotPCOA("98_lt_phylip_amazon.pcoa.axes",design=="Amazon_Data.design") 
this plots the first 2 axes in a 2d scatterplot in black and white 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
