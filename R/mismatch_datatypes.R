mismatch_datatypes = function (source, target){
  #finds common columns and checks if datatypes match
  #returns a named character vector

  dt_src = sapply(source,  class)
  dt_tgt = sapply(target, class)
  src_and_tgt = dt_src %in% dt_tgt
  char_vect = dt_src[!src_and_tgt]

  return(char_vect)

}
