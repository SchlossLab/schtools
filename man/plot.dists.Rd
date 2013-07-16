\name{plot.dists}
\alias{plot.dists}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot distances
}
\description{
This function will plot a barplot using the output form either within.dist or between.dist.  Many of the same parameters as barplot() can be used with this function.
}
\usage{
plot.dists(list, space = NULL, col = c(), ylim = c(0, 1), ylab = "Average Distance", xlab = "", main = c())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list}{
a list of vectors containing the distances between or within samples
}
  \item{space}{
the amount of space (as a fraction of the average bar width) left before each bar. May be given as a single number or one number per bar. If height is a matrix and beside is TRUE, space may be specified by two numbers, where the first is the space between bars in the same group, and the second the space between the groups. If not given explicitly, it defaults to c(0,1) if height is a matrix and beside is TRUE, and to 0.2 otherwise.
}
  \item{col}{
a vector of colors for the bars or bar components. By default, grey is used if height is a vector, and a gamma-corrected grey palette if height is a matrix.
}
  \item{ylim}{
limits for the y axis.
}
  \item{ylab}{
a label for the y axis.
}
  \item{xlab}{
a label for the x axis.
}
  \item{main}{
overall title for the plot
}
}
\details{
The only necessary function is 'list'.  If no 'col' is specified, then the function will randomly generate colors.  For the rest of the parameters the defaults work well for quickly visualizing distances.
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
# Plot output from other function using random colors and default parameters
#plot.dists(btwn.dists)	

# Specifying colors and main title
#plot.dists(btwn.dists, col=c('blue','red','green'), main='Title of Chart')


## The function is currently defined as
function (list, space = NULL, col = c(), ylim = c(0, 1), ylab = "Average Distance", 
    xlab = "", main = c()) 
{
    avgs <- c()
    for (i in 1:length(list)) {
        avgs[i] <- mean(list[[i]])
    }
    if (length(col) == 0) {
        col <- rgb(runif(length(list)), runif(length(list)), 
            runif(length(list)))
    }
    xcor <- barplot(avgs, names.arg = names(list), space = space, 
        ylim = ylim, col = col, xlab = xlab, ylab = ylab, main = main)
    for (i in 1:length(list)) {
        arrows(x0 = xcor[i], x1 = xcor[i], y0 = mean(list[[i]]), 
            y1 = mean(list[[i]]) + sd(list[[i]]), angle = 90)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
