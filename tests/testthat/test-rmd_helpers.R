test_that("paste_oxford_list() works for vectors & lists", {
  expect_equal(paste_oxford_list(1:3), '1, 2, and 3')
  expect_equal(paste_oxford_list(as.list(1:3)), '1, 2, and 3')
  expect_equal(paste_oxford_list(1:2), '1 and 2')
  expect_equal(paste_oxford_list(1), '1')
})
