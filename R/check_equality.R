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
  column_name = c(column_name, paste("i.",column_name,sep=""))
  column1 = column_name[1]
  column2 = column_name[2]
  dt = cbind(datatable[,..keys],datatable[,..column_name],
             mismatch = (datatable[,..column1] != datatable[,..column2])
  )
  #find mismathing column index
  mismatch_column_position = length(keys) + length(column_name) + 1
  return(dt[dt[[mismatch_column_position]],])
}
