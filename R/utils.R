#' dplyr pipe
#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

## make R CMD CHECK shut up about the dot `.`
## See: \url{https://github.com/tidyverse/magrittr/issues/29}
utils::globalVariables(c("."))

#' Install & load packages
#'
#' @param ... names of packages to install
#' @export
load_deps <- function(...) {
  deps <- c(...)
  not_installed <- names(Filter(isFALSE, sapply(deps, requireNamespace,
                                                quietly = TRUE)))
  utils::install.packages(not_installed, quiet = TRUE, dependencies = TRUE,
                   repos = "http://cran.us.r-project.org")
  invisible(sapply(deps, library, verbose = FALSE, character.only = TRUE))
}
