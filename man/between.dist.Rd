\name{between.dist}
\alias{between.dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculating distances between groups
}
\description{
Imports a distance matrix and a '.design' file, and uses them to return statistics about the distances between defined groups of samples.  Prints statistics and when asigned to a variable, returns a list of the distances between designated groups to be used for further analysis.
}
\usage{
between.dist(dist, design, groups, subgroups=FALSE)
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
If the dist paramater is a phylip file, then the function will start by using the read.dist function to import the distance matrix.  The function will then parse distance matrix into smaller matrices each containing the columns from one group of interest and the rows of another.  The distances in each of the smaller matrices are then used to calculated the minimum, maximum, median, mean, and standard deviation of the distances between each group.  The statistics are printed to the screen. 
	
	When assigned to a variable, the function will output a list of vectors,  each containing all of the distances between the two groups.  The vectors in the list are named "'group1' vs 'group2', "'group1' vs 'group3, etc.  This list output can used for further statistical analysis of the distances, such as a Wilcoxon test, or can be input in the plot.dists function to make a barplot of the distances.
	
	The function is designed such that each pair of groups is only compared once and the distances within a group are not returned. For distances within a group use the within.dist function.
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
Since the output of this function is a list, pulling out a vector from the list is done using my.list[[1]].  To get a single distance assign my.list[[1]] to a variable (i.e. vect1<-my.list[[1]]) then specify the component of the list using vect1[1].  I only say this because it may not be obvious to people who aren't experienced R users.

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
within.dist for calculating distances within groups
plot.dist for plotting the output from of between.dist and within.dist
}
\examples{
#Using a phylip file as input
#between.dist('~/Desktop/germfree.sq.dist','~/Desktop/germfree.design', c('early','late'))
	
#Using a distance matrix that has already been read in using read.dist
#between.dist(gf.sq.dist,'~/Desktop/germfree.design', c('early','late'))
	
#Calculating distances between combinations of groups
#between.dist('~/Desktop/germfree.sq.dist','~/Desktop/germfree.design', 'all')
	
#Plotting the output
#btwn.list<-between.dist('~/Desktop/germfree.sq.dist','~/Desktop/germfree.design', c('early','late'))
#plot.dists(btwn.list)


## The function is currently defined as
function (dist, design, groups) 
{
    if (length(groups) == 1) {
        options(warn = (-1))
        if (groups == "all") {
            if (length(design) == 1) {
                dsn <- read.table(design)
            }
            if (length(design) > 1) {
                dsn <- design
            }
            groups <- as.vector(unique(dsn[, 2]))
            options(warn = (0))
        }
    }
    one.between <- function(dist, design, group1, group2) {
        if (typeof(dist) == "character") {
            dist <- read.dist(dist)
        }
        if (length(design) == 1) {
            design <- read.design(design)
        }
        get.dists <- function(dist, Detailsdesign, group1, group2) {
            group1 <<- group1
            group2 <<- group2
            get.grp1 <- function(d, g) {
                c1 <- 1
                grp1 <- c()
                for (i in 1:nrow(d)) {
                  if (d[i, 2] == g) {
                    grp1[c1] <- d[i, 1]
                    c1 <- c1 + 1
                  }
                }
                invisible(grp1)
            }
            get.grp2 <- function(d, g) {
                c2 <- 1
                grp2 <- c()
                for (i in 1:nrow(d)) {
                  if (d[i, 2] == g) {
                    grp2[c2] <- d[i, 1]
                    c2 <- c2 + 1
                  }
                }
                invisible(grp2)
            }
            grp1 <- get.grp1(design, group1)
            grp2 <- get.grp2(design, group2)
            get.rows <- function(dist, grp) {
                rows <- dist[grp[1], ]
                for (i in 2:length(grp)) {
                  rows <- rbind(rows, dist[grp[i], ])
                }
                invisible(rows)
            }
            rows <- get.rows(dist, grp1)
            get.rows <- function(dist, grp) {
                matrix <- dist[, grp[1]]
                for (i in 2:length(grp)) {
                  matrix <- cbind(matrix, dist[, grp[i]])
                }
                invisible(matrix)
            }
            matrix <- get.rows(rows, grp2)
            vect <- as.vector(matrix)
            invisible(vect)
        }
        dists <- get.dists(dist, design, group1, group2)
        cat(sprintf("Stats for distances between \%s and \%s", 
            group1, group2), "\n", sprintf("Minimum: \%f", min(dists)), 
            "\n", sprintf("Maximum: \%f", max(dists)), "\n", sprintf("Median: \%f", 
                median(dists)), "\n", sprintf("Mean: \%f", mean(dists)), 
            "\n", sprintf("Std. Dev.: \%f", sd(dists)), "\n", 
            "\n", sep = "")
        return(dists)
    }
    dist.vect <- c()
    c <- 1
    dist.list <- vector(mode = "list", length = ((length(groups)^2 - 
        length(groups))/2))
    list.names <- c()
    for (i in 1:(length(groups) - 1)) {
        for (j in (i + 1):length(groups)) {
            list.names[c] <- paste(groups[i], "vs", groups[j])
            dist.list[[c]] <- one.between(dist, design, groups[i], 
                groups[j])
            c <- c + 1
        }
    }
    names(dist.list) <- list.names
    invisible(dist.list)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
