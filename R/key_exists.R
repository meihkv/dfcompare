key_exists = function(df, keys) {
  #checks if keys exist in columns of dataframe.
  #If not, then it prints an error message with missing keys.
  #returns a boolean

  result = keys %in% colnames(df)
  if (sum(result) == length(keys)){
    return(TRUE)
  }
  else{
    df_name = deparse(substitute(df))
    error_msg = sprintf('%s specified key(s) not found in %s: %s',sum(!result), df_name, paste(keys[!result],collapse=", "))
    stop(error_msg)
    return(FALSE)
  }
}
