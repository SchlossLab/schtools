#' Checks whether a number is near to a whole number
#'
#' @param x a numeric
#'
#' @return `TRUE` or `FALSE`
#' @export
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#'
#' @examples
#' is_nearly_whole(.Machine$double.eps^0.5)
#' is_nearly_whole(.Machine$double.eps^0.6)
#' is_nearly_whole(1)
is_nearly_whole <- function(x) {
  abs(x - round(x)) < .Machine$double.eps^0.5
}

#' Check whether two numeric vectors are close enough for gov't work.
#'
#' This is like `dplyr::near()` except with much less precision.
#'
#' @param x a numeric vector
#' @param y another numeric vector
#' @param tol tolerance (default: `10^-3`.)
#'
#' @return `TRUE` if all numbers are near enough within the tolerance, otherwise `FALSE`
#' @export
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#'
#' @examples
#'
#' close_enough(0.0004, 0)
#' close_enough(0.8887, 0.8884)
#' close_enough(1, 2)
close_enough <- function(x, y, tol = 10^-3) {
  all(dplyr::near(x, y, tol = tol))
}

#' Check whether all elements given are sorted in non-descending order
#'
#' @param ... anything!
#'
#' @return `TRUE` if the elements are sorted in non-descending order, otherwise `FALSE`
#' @export
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#'
#' @examples
#'
#' is_nondesc(1, 2, 3)
#' is_nondesc(c(1, 2), 3)
#' is_nondesc(6, 4, 1)
#' is_nondesc("a", "b", "c")
#' is_nondesc(c("z", "y"))
is_nondesc <- function(...) {
  things <- c(...)
  if (length(things) < 1) {
    warning("Zero elements were given to `is_nondesc()`")
  }
  first <- things[1]
  last <- things[length(things)]
  !is.unsorted(things) & first <= last
}

#' Install & load packages
#' @param ... package names to install & load
#' @export
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
load_deps <- function(...) {
  deps <- c(...)
  not_installed <- names(Filter(isFALSE, sapply(deps, requireNamespace,
    quietly = TRUE
  )))
  utils::install.packages(not_installed,
    quiet = TRUE, dependencies = TRUE,
    repos = "http://cran.us.r-project.org"
  )
  invisible(sapply(deps, library, verbose = FALSE, character.only = TRUE))
}

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
#'   log file [default: TRUE].
#'
#' @export
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#' @examples
#' # The Snakemake doesn't exist, so nothing happens
#' log_snakemake(quiet = FALSE)
log_snakemake <- function(quiet = TRUE) {
  if (exists("snakemake")) {
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
