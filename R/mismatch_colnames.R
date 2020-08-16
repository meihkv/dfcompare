#' Searches for uncommon column names between two data frames
#'
#' This is a private function for dfcompare.
#'
#' @param source data table of source
#' @param target data table of target
#' @return a list of data tables containing uncommon column names
#'
mismatch_colnames = function (source, target, src_name="source", tgt_name="target") {

  col_src = colnames(source)
  col_tgt = colnames(target)

  src_not_tgt = col_src[!(col_src %in% col_tgt)]
  tgt_not_src = col_tgt[!(col_tgt %in% col_src)]

  uncommon = list(
    data.table::data.table(src_not_tgt),
    data.table::data.table(tgt_not_src)
  )

  names(uncommon)[[1]] = paste(src_name,"not in",tgt_name,sep=" ")
  names(uncommon)[[2]] = paste(tgt_name,"not in",src_name,sep=" ")

  return(uncommon)
}

