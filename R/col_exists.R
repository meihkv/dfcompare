#' Checks if column names exist in columns of dataframe.
#'
#' This is a private function for dfcompare.
#'
#' It is currently used to detect if the specified keys are in the data frame.
#' If not found, it prints and error message indicating the missing columns.
#'
#' @param df data frame to be tested
#' @param colname a vector of character strings to check
#' @return True if found, false if not

col_exists = function(df, colname) {

  result = colname %in% colnames(df)
  if (sum(result) == length(colname)){
    return(TRUE)
  }
  else{
    df_name = deparse(substitute(df))
    error_msg = sprintf('%s specified column(s) not found in %s: %s',sum(!result), df_name, paste(colname[!result],collapse=", "))
    print(error_msg)
    return(FALSE)
  }
}
