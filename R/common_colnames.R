#' Searches for common column names between two data frames
#'
#' This is a private function for dfcompare.
#'
#' @param source data frame of source
#' @param target data frame of target
#' @return a vector of common column names

common_colnames = function(source, target) {
  col_src = colnames(source)
  col_tgt = colnames(target)
  char_vect = intersect(col_src,col_tgt)
  return(char_vect)
}
