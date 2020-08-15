set.seed(10)
random = function(x){
  result = rnorm(1)
  if(result<0){
    return(x[1])
  }
  else{
    return(result*x[1])
  }
}

target = as.data.table(longley)
target$GNP.deflator = sapply(target$GNP.deflator,random)
target$Population = sapply(target$Population,random)

source= as.data.table(longley)

src_and_tgt = source[target, on = 'Year']

result = check_equality('GNP.deflator',src_and_tgt,'Year')

test_that("returns names", {
  expect_that(result, is_a("data.table"))
  expect_equal(nrow(result),8)
})


