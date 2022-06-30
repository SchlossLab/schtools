
test_that("is_nearly_whole() works", {
  expect_true(is_nearly_whole(.Machine$double.eps))
  expect_true(is_nearly_whole(0))
  expect_true(is_nearly_whole(1))
  expect_false(is_nearly_whole(.Machine$double.eps^0.5))
  expect_false(is_nearly_whole(2100.05))
  expect_equal(is_nearly_whole(NA), NA)
})

test_that("close_enough() works", {
  expect_true(close_enough(0.0004, 0))
  expect_true(close_enough(0.8887, 0.8884))
  expect_false(close_enough(1, 2))
  expect_equal(close_enough(1, NA), NA)
})

test_that("is_nondesc() works", {
  expect_true(is_nondesc(1, 2, 3))
  expect_true(is_nondesc(c(1, 2), 3))
  expect_false(is_nondesc(6, 4, 1))
  expect_true(is_nondesc("a", "b", "c"))
  expect_false(is_nondesc(c("z", "y")))
  expect_true(is_nondesc(1))
  expect_warning(is_nondesc(c()), "Zero elements were given to `is_nondesc\\(\\)`")
  expect_true(is_nondesc(1, 2, 3, 4, 5, 6, 7))
})

dummy_snakemake <- function(rule = 'test_rule',
                            log_filename = 'tests/tmp.log',
                            create_log = FALSE,
                            quiet = FALSE) {
    if (isTRUE(create_log)) {
        log_filelist <- list(log_filename)
    } else {
        log_filelist <- list()
    }
    # setup Snakemake S4 object
    setClass("Snakemake", representation(log = "list", rule = 'character'))
    # create snakemake object in global environment
    snakemake <<- new("Snakemake", log = log_filelist, rule = rule)
    # run the function we're actually interested in testing
    log_snakemake(quiet = quiet)
    message('This is a message')
    # close all sinks
    for (i in seq_len(sink.number())) {
        sink(NULL)
    }
}

test_that("log_snakemake() prints messages when quiet=FALSE", {
    expect_message(log_snakemake(quiet = FALSE), "No Snakemake object exists")

    # TODO: this works in the console but not inside test_that()
    # expect_message(
    #     dummy_snakemake(create_log = FALSE, quiet = FALSE),
    #     "No log file was specified in the Snakemake rule test_rule"
    # )

    # TODO: figure out how to prevent testthat from hijacking `sink()`?
    # log_filename <- 'tests/tmp.log'
    # expect_message(
    #     dummy_snakemake(create_log = TRUE, quiet = FALSE,
    #                     log_filename = log_filename),
    #     paste('Saving output to', log_filename)
    # )
})

