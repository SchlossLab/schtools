\name{stackBarSDP}
\alias{StackBar}
\alias{StackBarSDP}
\alias{stackbar}
\alias{stackBarSDP}
\title{
Stacked Bar Charts For Shared And Design
}
\description{
Given A shared file and a design file from mothur create a stacked bar chart by group.
}
\usage{
stackBarSDP(shared, design, cutoff, legend.space = 1.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shared}{
     The Shared File from mothur you wish to use. ie example.shared
}
 \item{design}{
     The Design file which contains the partitions you wish to use. ie example.design
 }
 \item{cutoff}{
    The minimum percentage of any partition a group can be before being lumped into the other column. ie. cutoff=.015 
 }
 \item{legend.space}{
    How far to the right you want the legend to be. Default is 1.1.
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
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
The mothur wiki \code{\link{http://www.mothur.org/wiki/Main_Page}}
}
\examples{
stackBarSDP("example.subsample.shared","example.design",.01)
stackBarSDP("example2.subsample.shared","example2.design",.05,legend.space = 1.6)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ mothur }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
