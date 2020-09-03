#' Read in lower left triangular matrix from file
#'
#' @param dist_filename filename of lower left triangular matrix (.dist)
#'
#' @return distance matrix as a tibble
#' @export
#' @author Nick Lesniak, \email{nlesniak@@umich.edu}
#'
#' @examples
#' dist_filepath <- system.file("extdata",
#'                              "sample.final.thetayc.0.03.lt.ave.dist",
#'                              package = "mothuR")
#' dist_tbl <- read_dist(dist_filepath)
#' head(dist_tbl)
read_dist <- function(dist_filename) {
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
  colnames(distance_matrix) <- c('rows', distance_matrix$X1[-matrix_dim])
  # convert to long form and eliminate NAs (upper right of triangle)
  return(
    distance_matrix %>%
      tidyr::pivot_longer(cols = -.data[['rows']],
                          values_to = 'distances',
                          names_to = 'columns'
                          ) %>%
      dplyr::filter(!is.na(.data[['distances']]))
  )
}
