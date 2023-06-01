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

#' Check whether two numeric vectors are close enough
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
