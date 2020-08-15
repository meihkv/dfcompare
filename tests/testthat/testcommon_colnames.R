
test_that("common columns are found", {
  expect_setequal(common_colnames(iris,iris), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species"))
})

test_that("no common columns are found", {
  expect_setequal(common_colnames(iris,cars), as.character())
})
