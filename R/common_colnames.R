common_colnames = function(source, target) {
  col_src = colnames(source)
  col_tgt = colnames(target)
  char_vect = intersect(col_src,col_tgt)
  return(char_vect)
}
