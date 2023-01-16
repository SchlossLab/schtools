dummy_snakemake <- function(rule = "test_rule",
                            log_filename = "tests/tmp.log",
                            create_log = FALSE,
                            quiet = FALSE) {
  if (isTRUE(create_log)) {
    log_filelist <- list(log_filename)
  } else {
    log_filelist <- list()
  }
  # setup Snakemake S4 object
  setClass("Snakemake", representation(log = "list", rule = "character"))
  # create snakemake object in global environment
  snakemake <<- new("Snakemake", log = log_filelist, rule = rule)
  # run the function we're actually interested in testing
  log_snakemake(quiet = quiet)
  message("This is a message")
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

test_wildcards <- function() {
  setClass("Snakemake", representation(rule = "character", wildcards = "list"))
  snakemake <<- new("Snakemake",
    rule = "train_ml",
    wildcards = list("otu-mini-bin", "glmnet", "101",
      dataset = "otu-mini-bin",
      method = "glmnet",
      seed = 101
    )
  )
  return(get_wildcards_tbl())
}

test_that("get_wildcards_tbl() works", {
  expect_equal(
    test_wildcards(),
    structure(
      list(
        dataset = "otu-mini-bin",
        method = "glmnet",
        seed = 101
      ),
      row.names = c(NA, -1L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )
})
