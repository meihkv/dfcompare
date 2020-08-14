#' Checks if two vectors of the same name of equal length have equal values
#'
#' This is a private function for dfcompare.
#' It requires a data table containing the name of the column and the
#' prefix "i." + name of the column to be compared with each other.
#'
#' @param column_name name of column to be tested
#' @param keys vector name of columns to be included as reference
#' @param datatable name of data table containing the columns to be tested
#' @return data table with only the tested columns and a mismatch column

check_equality = function (column_name, datatable, keys) {

  src_col_name = column_name
  tgt_col_name = paste("i.",column_name,sep="")

  sorted_column_names = c(keys, src_col_name, tgt_col_name)

  tested_columns = cbind(datatable[,..sorted_column_names])

  #Return only mismatches
  return(tested_columns[tested_columns[[2]]!=tested_columns[[3]],])
}
