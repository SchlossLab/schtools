
#' Save output, messages, warnings, and errors to the Snakemake log file
#'
#' This function checks whether a log file was specified in the Snakemake rule.
#' If so, it directs any output, messages, warnings, or errors to the
#' rule-specific log file.
#' See the Snakemake documentation on [log files](https://snakemake.readthedocs.io/en/stable/snakefiles/rules.html#log-files)
#' and [R scripts](https://snakemake.readthedocs.io/en/stable/snakefiles/rules.html#r-and-r-markdown)
#' for more details.
#'
#' @param quiet Silence messages about the status of the snakemake object and
#'   log file (default: `TRUE`).
#'
#' @export
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#' @examples
#' # The Snakemake object doesn't exist, so nothing happens
#' log_snakemake(quiet = FALSE)
log_snakemake <- function(quiet = TRUE) {
  if (exists("snakemake")) {
    if (FALSE) {
      snakemake <- NULL
    } # silences warning "no visible binding for global variable ‘snakemake’
    if (length(snakemake@log) > 0) {
      log_filepath <- snakemake@log[1][[1]]
      if (isFALSE(quiet)) {
        message(paste("Saving output to", log_filepath))
      }
      log <- file(log_filepath, open = "wt")
      sink(log, append = TRUE)
      sink(log, append = TRUE, type = "message")
    } else {
      if (isFALSE(quiet)) {
        message(
          paste(
            "No log file was specified in the Snakemake rule",
            snakemake@rule
          )
        )
      }
    }
  } else {
    if (isFALSE(quiet)) {
      message("No Snakemake object exists, so all output will print as usual")
    }
  }
}

#' Get the Snakemake wildcards as a tibble
#'
#' @return a tibble of wildcards, with columns as names and rows as values
#' @export
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
get_wildcards_tbl <- function() {
  if (!exists("snakemake")) {
    stop("No Snakemake object exists")
  } else if (length(snakemake@wildcards) == 0) {
    warning("The Snakemake object contains no wildcards")
  }
  wildcard_names <- snakemake@wildcards %>%
    names() %>%
    Filter(function(x) {
      nchar(x) > 0
    }, .)
  wildcards <- snakemake@wildcards[wildcard_names] %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(seed = as.numeric(seed))
  return(wildcards)
}
