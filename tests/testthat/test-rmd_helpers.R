test_that("paste_oxford_list() works for vectors & lists", {
  expect_equal(paste_oxford_list(1:3), "1, 2, and 3")
  expect_equal(paste_oxford_list(as.list(1:3)), "1, 2, and 3")
  expect_equal(paste_oxford_list(1:2), "1 and 2")
  expect_equal(paste_oxford_list(1), "1")
})

test_that("format_number() works for numbers & strings", {
  expect_equal(format_number(0.02), "0.02")
  expect_equal(format_number(.Machine$double.eps^0.5), "0.000000015")
  expect_equal(format_number(1000), "1,000")
  expect_equal(format_number(1000.06), "1,000.1")
  expect_equal(format_number(0.795123), "0.80")
  expect_equal(format_number(TRUE), "TRUE")
  expect_equal(format_number("this is a string"), "this is a string")
})

test_that("inline_hook() uses paste_oxford_list() and format_number() correctly", {
  expect_equal(
    inline_hook(seq(0, 3 * .Machine$double.eps^0.5, .Machine$double.eps^0.5)),
    "0, 0.000000015, 0.00000003, and 0.000000045"
  )
})

test_that("set_knitr_opts() works", {
  chunk <- list(
    tidy = TRUE,
    echo = FALSE,
    eval = TRUE,
    message = FALSE,
    warning = FALSE,
    cache = FALSE
  )
  set_knitr_opts()
  expect_equal(knitr::opts_chunk$get()[names(chunk)], chunk)
  expect_equal(knitr::knit_hooks$get("inline"), inline_hook)
})
