\name{rarePlot}
\alias{RarePlot}
/alias{RareFraction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Rarefaction.single data
}
\description{
Plot the .group.rarefraction data which is the output from mothur's rarefraction.single function. 
}
\usage{
rarePlot(file,groups,ylab= "Number of Different OTUs",xlab= "Number of Tags Sampled",
         pch= NA, xlim = NULL, ylim = NULL, error=FALSE, delNum=6)
}
\arguments{
  \item{file}{
    The .group.rarefaction file which you wish to plot. Only works for the output of rarefaction.single from mothur.
  }
  \item{groups}{
  The specific groups within x which you wish to plot, in list form c("Examplegroup1", "Examplegroup2'). either the names ie c("m24626d36", "Day3") or the number the group falls in the list of all groups. ie c(1,4,7) (the 1st 4th and 7th group). 
}
  \item{ylab}{
    Title for y axis. Defaults to "Number of Different OTUs"
}
  \item{xlab}{
    Title for x axis. Defaults to "Number of Tags Sampled"
}
  \item{pch}{
    Character string or vector of 1-characters or integers for plotting characters, see points. The first character is the plotting-character for the first plot, the second for the second, etc. The default is nothing. Change to NULL if you want the digits 1:9 then the lowercase and uppercase letters
}
  \item{xlim}{
    Range of x axis
}
  \item{ylim}{
    Range of y axis
}
  \item{error}{
    If error = TRUE then plot the Lower and Upperbounds for each group. 
}
  \item{delNum}{
    The number of excess characters before each name generally X0.03. or something similiar. Set delNum to the number you which to delete.
}
  }
\seealso{
The Rarefaction function on mothur's wiki \code{\link{http://www.mothur.org/wiki/Rarefaction.single}}, 
}
\examples{
RarePlot("example.groups.shared",c("Examplegroup1","Examplegroup2","ExampleGroup3"))
RarePlot("example2.groups.shared",c(14,11,30,"ExampleGroup4"),error=TRUE,pch=NULL)
}