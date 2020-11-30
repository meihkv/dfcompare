# dfcompare v.01

dfcompare is a simple data frame comparison tool.

## Installation
devtools::install_github('meihkv/dfcompare')

## Usage (under construction)

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

dfcompare(longley, target, 'Year', summary = FALSE)


## License
[MIT](https://choosealicense.com/licenses/mit/)
