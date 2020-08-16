#' Searches for mismatching datatypes between two data frames
#'
#' This is a private function for dfcompare.
#'
#' @param source data table of source
#' @param target data table of target
#' @return a data table of column names with mismatching data types
#'
mismatch_datatypes = function (source, target, src_name = "source", tgt_name = "target"){

  common = common_colnames(source,target)

  dt_src = sapply(source[,..common], typeof)
  dt_tgt = sapply(target[,..common], typeof)

  #Create a data table of dt_src and dt_tgt where they do not equal
  df = data.table::data.table(common,dt_tgt, dt_src)[which(dt_src != dt_tgt),]
  names(df) = c('Column', src_name, tgt_name)

  return(df)

}
