library(ggplot2)
plot_sovacool <- ggplot(mtcars) +
  aes(x = factor(gear), y = mpg, color = factor(cyl)) +
  geom_boxplot() +
  theme_sovacool()
plot_lucas <- ggplot(mtcars) +
  aes(x = mpg, y = wt, color = cyl) +
  geom_point() +
  theme_lucas()

test_that("theme_sovacool has no margins", {
  expect_equal(
    plot_sovacool$theme$legend.margin,
    margin(0, 0, 0, 0, unit = "pt")
  )
  expect_equal(
    plot_sovacool$theme$plot.margin,
    margin(0, 0, 0, 0, unit = "pt")
  )
  expect_equal(
    plot_sovacool$theme$legend.box.margin,
    margin(0, 0, 0, 0, unit = "pt")
  )
})
test_that("theme_lucas custom font works", {
  expect_equal(plot_lucas$theme$text$family, "PT Sans")
})
test_that("plots print without error or warning messages", {
  expect_invisible(print(plot_sovacool))
  expect_invisible(print(plot_lucas))
})
