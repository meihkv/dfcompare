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

check_equality = function (column_name, datatable, keys, dt_names) {

  src_col_name = column_name
  tgt_col_name = paste("i.",column_name,sep="")

  src_name = dt_names[1]
  tgt_name = dt_names[2]

  sorted_column_names = c(keys, src_col_name, tgt_col_name)

  #Subset columns from data table using sorted_column_names
  #Subset rows where data table source and target columns do not equal
  tested_columns = datatable[,..sorted_column_names][
                          datatable[,..sorted_column_names][[src_col_name]] !=
                          datatable[,..sorted_column_names][[tgt_col_name]]
                          ,]

  #Rename tested columns to reflect target and source names

  setnames(tested_columns,src_col_name,src_name)
  setnames(tested_columns,tgt_col_name,tgt_name)

  #Return only mismatches
  return(tested_columns)
}
