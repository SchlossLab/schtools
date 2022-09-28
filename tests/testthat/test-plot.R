library(ggplot2)
plot_sovacool <- ggplot(mtcars) +
    aes(x = mpg, y = wt, color = cyl) +
    geom_point() +
    theme_sovacool()
test_that("theme_sovacool works", {
    expect_equal(plot_sovacool$theme,
                 structure(
                     list(
                         line = structure(
                             list(
                                 colour = "black",
                                 size = 0.5,
                                 linetype = 1,
                                 lineend = "butt",
                                 arrow = FALSE,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_line",
                                       "element")
                         ),
                         rect = structure(
                             list(
                                 fill = "white",
                                 colour = "black",
                                 size = 0.5,
                                 linetype = 1,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_rect",
                                       "element")
                         ),
                         text = structure(
                             list(
                                 family = "",
                                 face = "plain",
                                 colour = "black",
                                 size = 11,
                                 hjust = 0.5,
                                 vjust = 0.5,
                                 angle = 0,
                                 lineheight = 0.9,
                                 margin = structure(
                                     c(0, 0, 0, 0),
                                     unit = 8L,
                                     class = c("margin",
                                               "simpleUnit", "unit", "unit_v2")
                                 ),
                                 debug = FALSE,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         title = NULL,
                         aspect.ratio = NULL,
                         axis.title = NULL,
                         axis.title.x = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = NULL,
                                 size = NULL,
                                 hjust = NULL,
                                 vjust = 1,
                                 angle = NULL,
                                 lineheight = NULL,
                                 margin = structure(
                                     c(2.75,
                                       0, 0, 0),
                                     unit = 8L,
                                     class = c("margin", "simpleUnit",
                                               "unit", "unit_v2")
                                 ),
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         axis.title.x.top = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = NULL,
                                 size = NULL,
                                 hjust = NULL,
                                 vjust = 0,
                                 angle = NULL,
                                 lineheight = NULL,
                                 margin = structure(
                                     c(0,
                                       0, 2.75, 0),
                                     unit = 8L,
                                     class = c("margin", "simpleUnit",
                                               "unit", "unit_v2")
                                 ),
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         axis.title.x.bottom = NULL,
                         axis.title.y = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = NULL,
                                 size = NULL,
                                 hjust = NULL,
                                 vjust = 1,
                                 angle = 90,
                                 lineheight = NULL,
                                 margin = structure(
                                     c(0, 2.75, 0, 0),
                                     unit = 8L,
                                     class = c("margin",
                                               "simpleUnit", "unit", "unit_v2")
                                 ),
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         axis.title.y.left = NULL,
                         axis.title.y.right = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = NULL,
                                 size = NULL,
                                 hjust = NULL,
                                 vjust = 0,
                                 angle = -90,
                                 lineheight = NULL,
                                 margin = structure(
                                     c(0, 0, 0, 2.75),
                                     unit = 8L,
                                     class = c("margin",
                                               "simpleUnit", "unit", "unit_v2")
                                 ),
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         axis.text = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = "grey30",
                                 size = structure(0.8, class = "rel"),
                                 hjust = NULL,
                                 vjust = NULL,
                                 angle = NULL,
                                 lineheight = NULL,
                                 margin = NULL,
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         axis.text.x = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = NULL,
                                 size = NULL,
                                 hjust = NULL,
                                 vjust = 1,
                                 angle = NULL,
                                 lineheight = NULL,
                                 margin = structure(
                                     c(2.2,
                                       0, 0, 0),
                                     unit = 8L,
                                     class = c("margin", "simpleUnit",
                                               "unit", "unit_v2")
                                 ),
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         axis.text.x.top = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = NULL,
                                 size = NULL,
                                 hjust = NULL,
                                 vjust = 0,
                                 angle = NULL,
                                 lineheight = NULL,
                                 margin = structure(
                                     c(0,
                                       0, 2.2, 0),
                                     unit = 8L,
                                     class = c("margin", "simpleUnit",
                                               "unit", "unit_v2")
                                 ),
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         axis.text.x.bottom = NULL,
                         axis.text.y = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = NULL,
                                 size = NULL,
                                 hjust = 1,
                                 vjust = NULL,
                                 angle = NULL,
                                 lineheight = NULL,
                                 margin = structure(
                                     c(0, 2.2, 0, 0),
                                     unit = 8L,
                                     class = c("margin",
                                               "simpleUnit", "unit", "unit_v2")
                                 ),
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         axis.text.y.left = NULL,
                         axis.text.y.right = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = NULL,
                                 size = NULL,
                                 hjust = 0,
                                 vjust = NULL,
                                 angle = NULL,
                                 lineheight = NULL,
                                 margin = structure(
                                     c(0, 0, 0, 2.2),
                                     unit = 8L,
                                     class = c("margin",
                                               "simpleUnit", "unit", "unit_v2")
                                 ),
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         axis.ticks = structure(
                             list(
                                 colour = "grey20",
                                 size = NULL,
                                 linetype = NULL,
                                 lineend = NULL,
                                 arrow = FALSE,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_line", "element")
                         ),
                         axis.ticks.x = NULL,
                         axis.ticks.x.top = NULL,
                         axis.ticks.x.bottom = NULL,
                         axis.ticks.y = NULL,
                         axis.ticks.y.left = NULL,
                         axis.ticks.y.right = NULL,
                         axis.ticks.length = structure(
                             2.75,
                             unit = 8L,
                             class = c("simpleUnit",
                                       "unit", "unit_v2")
                         ),
                         axis.ticks.length.x = NULL,
                         axis.ticks.length.x.top = NULL,
                         axis.ticks.length.x.bottom = NULL,
                         axis.ticks.length.y = NULL,
                         axis.ticks.length.y.left = NULL,
                         axis.ticks.length.y.right = NULL,
                         axis.line = structure(list(), class = c("element_blank",
                                                                 "element")),
                         axis.line.x = NULL,
                         axis.line.x.top = NULL,
                         axis.line.x.bottom = NULL,
                         axis.line.y = NULL,
                         axis.line.y.left = NULL,
                         axis.line.y.right = NULL,
                         legend.background = structure(
                             list(
                                 fill = NULL,
                                 colour = NA,
                                 size = NULL,
                                 linetype = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_rect", "element")
                         ),
                         legend.margin = structure(
                             c(0, 0, 0, 0),
                             unit = 8L,
                             class = c("margin",
                                       "simpleUnit", "unit", "unit_v2")
                         ),
                         legend.spacing = structure(
                             11,
                             unit = 8L,
                             class = c("simpleUnit",
                                       "unit", "unit_v2")
                         ),
                         legend.spacing.x = NULL,
                         legend.spacing.y = NULL,
                         legend.key = structure(
                             list(
                                 fill = "white",
                                 colour = NA,
                                 size = NULL,
                                 linetype = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_rect",
                                       "element")
                         ),
                         legend.key.size = structure(
                             1.2,
                             unit = 3L,
                             class = c("simpleUnit",
                                       "unit", "unit_v2")
                         ),
                         legend.key.height = NULL,
                         legend.key.width = NULL,
                         legend.text = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = NULL,
                                 size = structure(0.8, class = "rel"),
                                 hjust = NULL,
                                 vjust = NULL,
                                 angle = NULL,
                                 lineheight = NULL,
                                 margin = NULL,
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         legend.text.align = NULL,
                         legend.title = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = NULL,
                                 size = NULL,
                                 hjust = 0,
                                 vjust = NULL,
                                 angle = NULL,
                                 lineheight = NULL,
                                 margin = NULL,
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         legend.title.align = NULL,
                         legend.position = "right",
                         legend.direction = NULL,
                         legend.justification = "center",
                         legend.box = NULL,
                         legend.box.just = NULL,
                         legend.box.margin = structure(
                             c(0,
                               0, 0, 0),
                             unit = 8L,
                             class = c("margin", "simpleUnit", "unit",
                                       "unit_v2")
                         ),
                         legend.box.background = structure(list(), class = c("element_blank",
                                                                             "element")),
                         legend.box.spacing = structure(
                             11,
                             unit = 8L,
                             class = c("simpleUnit",
                                       "unit", "unit_v2")
                         ),
                         panel.background = structure(
                             list(
                                 fill = "white",
                                 colour = NA,
                                 size = NULL,
                                 linetype = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_rect",
                                       "element")
                         ),
                         panel.border = structure(
                             list(
                                 fill = NA,
                                 colour = "grey20",
                                 size = NULL,
                                 linetype = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_rect",
                                       "element")
                         ),
                         panel.spacing = structure(
                             5.5,
                             unit = 8L,
                             class = c("simpleUnit",
                                       "unit", "unit_v2")
                         ),
                         panel.spacing.x = NULL,
                         panel.spacing.y = NULL,
                         panel.grid = structure(
                             list(
                                 colour = "grey92",
                                 size = NULL,
                                 linetype = NULL,
                                 lineend = NULL,
                                 arrow = FALSE,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_line",
                                       "element")
                         ),
                         panel.grid.major = NULL,
                         panel.grid.minor = structure(
                             list(
                                 colour = NULL,
                                 size = structure(0.5, class = "rel"),
                                 linetype = NULL,
                                 lineend = NULL,
                                 arrow = FALSE,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_line",
                                       "element")
                         ),
                         panel.grid.major.x = NULL,
                         panel.grid.major.y = NULL,
                         panel.grid.minor.x = NULL,
                         panel.grid.minor.y = NULL,
                         panel.ontop = FALSE,
                         plot.background = structure(
                             list(
                                 fill = NULL,
                                 colour = "white",
                                 size = NULL,
                                 linetype = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_rect",
                                       "element")
                         ),
                         plot.title = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = NULL,
                                 size = structure(1.2, class = "rel"),
                                 hjust = 0,
                                 vjust = 1,
                                 angle = NULL,
                                 lineheight = NULL,
                                 margin = structure(
                                     c(0, 0, 5.5, 0),
                                     unit = 8L,
                                     class = c("margin",
                                               "simpleUnit", "unit", "unit_v2")
                                 ),
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         plot.title.position = "panel",
                         plot.subtitle = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = NULL,
                                 size = NULL,
                                 hjust = 0,
                                 vjust = 1,
                                 angle = NULL,
                                 lineheight = NULL,
                                 margin = structure(
                                     c(0, 0, 5.5, 0),
                                     unit = 8L,
                                     class = c("margin",
                                               "simpleUnit", "unit", "unit_v2")
                                 ),
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         plot.caption = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = NULL,
                                 size = structure(0.8, class = "rel"),
                                 hjust = 1,
                                 vjust = 1,
                                 angle = NULL,
                                 lineheight = NULL,
                                 margin = structure(
                                     c(5.5, 0, 0, 0),
                                     unit = 8L,
                                     class = c("margin",
                                               "simpleUnit", "unit", "unit_v2")
                                 ),
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         plot.caption.position = "panel",
                         plot.tag = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = NULL,
                                 size = structure(1.2, class = "rel"),
                                 hjust = 0.5,
                                 vjust = 0.5,
                                 angle = NULL,
                                 lineheight = NULL,
                                 margin = NULL,
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         plot.tag.position = "topleft",
                         plot.margin = structure(
                             c(0,
                               0, 0, 0),
                             unit = 8L,
                             class = c("margin", "simpleUnit", "unit",
                                       "unit_v2")
                         ),
                         strip.background = structure(
                             list(
                                 fill = "grey85",
                                 colour = "grey20",
                                 size = NULL,
                                 linetype = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_rect",
                                       "element")
                         ),
                         strip.background.x = NULL,
                         strip.background.y = NULL,
                         strip.placement = "inside",
                         strip.text = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = "grey10",
                                 size = structure(0.8, class = "rel"),
                                 hjust = NULL,
                                 vjust = NULL,
                                 angle = NULL,
                                 lineheight = NULL,
                                 margin = structure(
                                     c(4.4, 4.4, 4.4, 4.4),
                                     unit = 8L,
                                     class = c("margin",
                                               "simpleUnit", "unit", "unit_v2")
                                 ),
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         strip.text.x = NULL,
                         strip.text.y = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = NULL,
                                 size = NULL,
                                 hjust = NULL,
                                 vjust = NULL,
                                 angle = -90,
                                 lineheight = NULL,
                                 margin = NULL,
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         ),
                         strip.switch.pad.grid = structure(
                             2.75,
                             unit = 8L,
                             class = c("simpleUnit",
                                       "unit", "unit_v2")
                         ),
                         strip.switch.pad.wrap = structure(
                             2.75,
                             unit = 8L,
                             class = c("simpleUnit",
                                       "unit", "unit_v2")
                         ),
                         strip.text.y.left = structure(
                             list(
                                 family = NULL,
                                 face = NULL,
                                 colour = NULL,
                                 size = NULL,
                                 hjust = NULL,
                                 vjust = NULL,
                                 angle = 90,
                                 lineheight = NULL,
                                 margin = NULL,
                                 debug = NULL,
                                 inherit.blank = TRUE
                             ),
                             class = c("element_text",
                                       "element")
                         )
                     ),
                     class = c("theme", "gg"),
                     complete = TRUE,
                     validate = TRUE
                 ))
})
