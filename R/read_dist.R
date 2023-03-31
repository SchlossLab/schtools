#' @name read_dist
#' @title Read lower triangular distance files as tibbles and matrices
NULL

#' @describeIn read_dist Read in lower left triangular matrix as a long tibble
#'
#' Assumes the file is a phylip-formatted lower left triangular matrix
#' as described in \url{https://mothur.org/wiki/phylip-formatted_distance_matrix/}
#'
#' @param filename file name of a lower left triangular matrix (`.dist`)
#'
#' @return matrix as a `tibble` in long format
#' @export
#' @author Nick Lesniak, \email{nlesniak@@umich.edu}
#'
#' @examples
#' dist_filepath <- system.file("extdata",
#'   "sample.final.thetayc.0.03.lt.ave.dist",
#'   package = "schtools"
#' )
#' dist_tbl <- read_dist(dist_filepath)
#' head(dist_tbl)
read_lt_as_tbl <- function(filename) {
  distances <- rows <- NULL
  # TODO: input validation - make sure file has expected format & throw errors if it doesn't
  # read in the first row to determine the matrix dimensions
  matrix_dim <-
    as.numeric(utils::read.table(dist_filename, nrows = 1, as.is = TRUE))
  # read in all the data from the lower triangle (exclude the first which is the matrix dim)
  distance_matrix <- utils::read.table(dist_filename,
    fill = TRUE,
    skip = 1,
    col.names = c(as.character(1:matrix_dim)),
    stringsAsFactors = F
  )
  # add column names based on row names
  colnames(distance_matrix) <- c("rows", distance_matrix$X1[-matrix_dim])
  # convert to long form and eliminate NAs (upper right of triangle)
  return(
    distance_matrix %>%
      tidyr::pivot_longer(
        cols = -rows,
        values_to = "distances",
        names_to = "columns"
      ) %>%
      dplyr::filter(!is.na(distances))
  )
}

#' @describeIn read_dist Read in lower left triangular matrix
#'
#'
#' @return matrix
#' @export
#' @author Pat Schloss, \email{pschloss@@umich.edu}
read_lt_mat <- function(filename){

    file <- scan(file_name,
                 what=character(),
                 quiet=TRUE,
                 sep="\n")

    n_samples <- as.numeric(file[1])
    file <- file[-1]

    file_split <- strsplit(file, "\t")

    fill_in <- function(x, length){
        c(x, rep("0", length - length(x)))
    }

    filled <- lapply(file_split, fill_in, length=n_samples + 1)

    samples_distance_matrix <- do.call(rbind, filled)

    samples <- samples_distance_matrix[,1]

    dist_matrix <- samples_distance_matrix[,-1]
    dist_matrix <- matrix(as.numeric(dist_matrix), nrow=n_samples)

    if(sum(dist_matrix[upper.tri(dist_matrix)]) == 0){
        dist_matrix <- dist_matrix+t(dist_matrix)
    }

    rownames(dist_matrix) <- samples
    colnames(dist_matrix) <- samples

    return(dist_matrix)
}
