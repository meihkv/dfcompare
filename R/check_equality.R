check_equality = function (column, datatable) {
  column = c(column, paste("i.",column,sep=""))
  column1 = column[1]
  column2 = column[2]
  cbind(datatable[,..column], mismatch = datatable[,..column1] != datatable[,..column2])
}
