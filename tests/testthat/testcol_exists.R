
test_that("column exists", {
  expect_true(col_exists(iris,'Species'))
})


test_that("column doesn't exist", {
  expect_false(col_exists(iris,'test me'))
})


test_that("column doesn't exist", {
  expect_false(col_exists(iris,c('test me','Species')))
})
