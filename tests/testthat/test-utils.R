
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
