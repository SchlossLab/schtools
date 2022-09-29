#' Kelly's go-to theme for ggplot2
#'
#' Uses `ggplot2::theme_bw()` and removes margins.
#'
#' @return list of ggproto objects
#'
#' @export
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars) +
#'   aes(x = mpg, y = wt, color = cyl) +
#'   geom_point() +
#'   theme_sovacool()
theme_sovacool <- function() {
  return(list(
    ggplot2::theme_bw(),
    ggplot2::theme(
      legend.margin = ggplot2::margin(0, 0, 0, 0, unit = "pt"),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0, unit = "pt"),
      plot.margin = ggplot2::margin(0, 0, 0, 0, unit = "pt")
    )
  ))
}
