\name{wilcox.dist}
\alias{wilcox.dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
 compares every pair of distances with everyother pair with wilcox.dist. EG if there are 3 groups A B and C wilcox.dist outputs the wilcox rank- 
 sum test of AB vs AC, AB vs BC and AC vs BC
}
\usage{
wilcox.dist(dist,design,groups,subgroups=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist}{
  Either a file containing a phylip formatted distance matrix with ThetaYC, UniFrac, or some other distance metric between samples or a variable representing the distance matrix that was imported using the read.dist function.
  }
  \item{design}{
 a '.design' file, i.e. a file containing two columns separated by a tab, with the sample names in the left column and the name of their group in the right column.
  }
  \item{groups}{
  a vector of strings containing the names of the groups to be analyzed.  There is no limit to the number of groups that can be analyzed.  The parameter 'all' can be used to analyze the relationships between all groups.
  }
  \item{subgroups}{
   a logical value determing whether to use the group column or the subgroup column in the analysis (FALSE is groups TRUE is subgroups). (which subgroups to use are determined by the groups vector.)
  }
}
\details{
%%  ~~ If necessary, more details than the description  above ~~
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
Andrew Hanna (ahanna12345@gmail.com) 
Nielson Baxter (ntbaxter@umich.edu)

}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Get the wilcox dists of all of the subgroups.  
wilcox.dist("germfree.sq.dist","germfreesubgroups.design",groups='all',subgroups=T)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
