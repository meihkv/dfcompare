check_equality = function (column_name, datatable) {
  column_name = c(column_name, paste("i.",column_name,sep=""))
  column1 = column_name[1]
  column2 = column_name[2]
  dt = cbind(datatable[,..column_name],
             mismatch = (datatable[,..column1] != datatable[,..column2])
  )
  return(dt[dt[[3]],])
}
