test_that("read_dist works on example file", {
  tbl_head <- structure(list(
    rows = c("104_1_D1", "104_1_D10", "104_1_D10", "104_1_D2", "104_1_D2", "104_1_D2"),
    columns = c("104_1_D0", "104_1_D0", "104_1_D1", "104_1_D0", "104_1_D1", "104_1_D10"),
    distances = c(0.892663, 0.253927, 0.921823, 0.874497, 0.108682, 0.90357)
  ),
  row.names = c(NA, -6L),
  class = c("tbl_df", "tbl", "data.frame")
  )
  tbl_tail <- structure(list(
    rows = c(
      "98_3_Dminus1", "98_3_Dminus1", "98_3_Dminus1",
      "98_3_Dminus1", "98_3_Dminus1", "98_3_Dminus1"
    ),
    columns = c("98_3_D4", "98_3_D5", "98_3_D6", "98_3_D7", "98_3_D8", "98_3_D9"),
    distances = c(0.659194, 0.697918, 0.717304, 0.621887, 0.645434, 0.638572)
  ),
  row.names = c(NA, -6L),
  class = c("tbl_df", "tbl", "data.frame")
  )
  dist_out <- read_dist(system.file("extdata",
    "sample.final.thetayc.0.03.lt.ave.dist",
    package = "schtools"
  ))
  expect_equal(head(dist_out), tbl_head)
  expect_equal(tail(dist_out), tbl_tail)
})
