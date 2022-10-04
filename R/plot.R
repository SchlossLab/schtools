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
#' Sarah's go-to theme for ggplot2
#'
#' Requires the `hrbrthemes` package and the `PT Sans` and `PT Sans Narrow`
#' fonts from Google Fonts.
#'
#' @return list of ggproto objects
#' @export
#' @author Sarah Lucas \email{salucas@@umich.edu}
#'
#' @examples
#' library(ggplot2)
#' library(showtext)
#'
#' # run once to download the PT Sans fonts
#' font_add_google(name = "PT Sans", family = "PT Sans")
#' font_add_google(name = "PT Sans Narrow", family = "PT Sans Narrow")
#' showtext_auto()
#'
#' # make a plot with theme_lucas()
#' ggplot(mtcars) +
#'   aes(x = mpg, y = wt, color = cyl) +
#'   geom_point() +
#'   theme_lucas()
theme_lucas <- function() {
    return(list(
        hrbrthemes::theme_ipsum(
            base_family       = "PT Sans",
            base_size         = 20,
            axis_title_family = "PT Sans Narrow",
            axis_title_size   = 20,
            axis_text_size    = 16,
            axis_title_just   = "c"
        ),
        ggplot2::theme(
                plot.title       = ggplot2::element_text(hjust = 0.5),
                plot.subtitle    = ggplot2::element_text(hjust = 0.5),
                plot.margin      = ggplot2::margin(15, 15, 15, 15),
                panel.grid.minor = ggplot2::element_blank(),
                panel.background = ggplot2::element_blank(),
                legend.position  = "bottom",
                legend.key       = ggplot2::element_rect(
                    fill = "white", color = "white", size = 4),
                legend.key.width = ggplot2::unit(2, "cm"),
                legend.text      = ggplot2::element_text(size = 12),
                plot.caption     = ggplot2::element_text(
                    color = "#85919b", hjust = 0, size = 12, face = "plain")
            )
    ))
}
