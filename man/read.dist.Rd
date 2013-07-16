\name{read.dist}
\alias{read.dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import a distance matrix
}
\description{
This function reads in a file containing a square distance matrix.
}
\usage{
read.dist(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
A phylip formatted distance matrix to be imported
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#read.dist(~/Desktop/'germfree.sq.dist')	


## The function is currently defined as
function (file) 
{
    pre.dist <- read.table(file, skip = 1)
    dist <- pre.dist[2:ncol(pre.dist)]
    colnames(dist) <- t(pre.dist[1])
    rownames(dist) <- t(pre.dist[1])
    invisible(dist)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
