test_that("paste_oxford_list() works for vectors & lists", {
  expect_equal(paste_oxford_list(1:3), "1, 2, and 3")
  expect_equal(paste_oxford_list(as.list(1:3)), "1, 2, and 3")
  expect_equal(paste_oxford_list(1:2), "1 and 2")
  expect_equal(paste_oxford_list(1), "1")
})

test_that("is_nearly_whole() works", {
  expect_true(is_nearly_whole(.Machine$double.eps))
  expect_true(is_nearly_whole(0))
  expect_true(is_nearly_whole(1))
  expect_false(is_nearly_whole(.Machine$double.eps^0.5))
  expect_false(is_nearly_whole(2100.05))
  expect_equal(is_nearly_whole(NA), NA)
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
    eval = TRUE, echo = FALSE, results = "markup", tidy = TRUE,
    tidy.opts = NULL, collapse = FALSE, prompt = FALSE, comment = "##",
    highlight = TRUE, strip.white = TRUE, size = "normalsize",
    background = "#F7F7F7", cache = FALSE, cache.path = "cache/",
    cache.vars = NULL, cache.lazy = TRUE, dependson = NULL, autodep = FALSE,
    cache.rebuild = FALSE, fig.keep = "high", fig.show = "asis",
    fig.align = "default", fig.path = "figure/", dev = NULL,
    dev.args = NULL, dpi = 72, fig.ext = NULL, fig.width = 7,
    fig.height = 7, fig.env = "figure", fig.cap = NULL, fig.scap = NULL,
    fig.lp = "fig:", fig.subcap = NULL, fig.pos = "", out.width = NULL,
    out.height = NULL, out.extra = NULL, fig.retina = 1, external = TRUE,
    sanitize = FALSE, interval = 1, aniopts = "controls,loop",
    warning = FALSE, error = TRUE, message = TRUE, render = NULL,
    ref.label = NULL, child = NULL, engine = "R", split = FALSE,
    include = TRUE, purl = TRUE
  )
  set_knitr_opts()
  expect_equal(knitr::opts_chunk$get(), chunk)
  expect_equal(knitr::knit_hooks$get("inline"), inline_hook)
})
