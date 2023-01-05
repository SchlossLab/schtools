
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
