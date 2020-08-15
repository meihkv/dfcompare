#' Searches for uncommon column names between two data frames
#'
#' This is a private function for dfcompare.
#'
#' @param source data frame of source
#' @param target data frame of target
#' @return a list of data frames containing uncommon column names
#'
mismatch_colnames = function (source, target) {
  #finds columns that are not in common with each other
  #returns a list of data frame vectors

  src_name = deparse(substitute(source,env=parent.frame()))
  tgt_name = deparse(substitute(target,env=parent.frame()))

  col_src = colnames(source)
  col_tgt = colnames(target)

  src_not_tgt = as.data.frame(col_src[!(col_src %in% col_tgt)])
  tgt_not_src = as.data.frame(col_tgt[!(col_tgt %in% col_src)])

  names(src_not_tgt)[1] = paste(src_name,"not in",tgt_name,sep=" ")
  names(tgt_not_src)[1] = paste(tgt_name,"not in",src_name,sep=" ")

  return(list(src_not_tgt, tgt_not_src))
}

