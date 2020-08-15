
test_that("returns names", {
  expect_that(mismatch_colnames(cars,iris), is_a("list"))
  expect_equal(length(mismatch_colnames(cars,iris)), 2)
})

test_that("returns nothing", {
  expect_that(mismatch_colnames(iris,iris), is_a("list"))
  expect_equal(length(mismatch_colnames(iris,iris)), 2)
  expect_equal(nrow(mismatch_colnames(iris,iris)[[1]]), 0)
  expect_equal(nrow(mismatch_colnames(iris,iris)[[2]]), 0)
})

