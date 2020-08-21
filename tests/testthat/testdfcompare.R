set.seed(10)

random = function(){
  result = rnorm(1)
  if(result<0){
    return(1)
  }
  else{
    return(result)
  }
}

target = data.frame(sapply(longley, function(x) x*replicate(n = nrow(longley), random())))

test_that("returns names", {
  result = dfcompare(longley, target, 'Year', summary = FALSE)
  expect_equal(length(result),
              8)
  expect_true(all.equal(result,result))
})


