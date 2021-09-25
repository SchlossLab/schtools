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
