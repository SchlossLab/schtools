\name{read.design}
\alias{read.design}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import a design file
}
\description{
This function reads in a design file. The leftmost column should have the names of all of the samples. The next column should have the name of the group they each fall into. You may also have an optional third column with the name of the subgroup they each fall into. The columns should be separated by a tab, and it should NOT have any headers.
}
\usage{
read.design(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
a design file formatted as described above.
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
#read.design(~/Desktop/'germfree.design')


## The function is currently defined as
function (file) 
{
    design <- read.table(file)
    design <- matrix(unlist(design), ncol = 2)
    colnames(design) <- c("sample", "group")
    invisible(design)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
