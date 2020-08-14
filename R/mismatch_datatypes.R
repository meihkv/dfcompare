mismatch_datatypes = function (source, target){
  #finds common columns and checks if datatypes match
  #returns a data frame vector
  src_name = deparse(substitute(source,env=parent.frame()))
  tgt_name = deparse(substitute(target,env=parent.frame()))

  common = common_colnames(source,target)

  dt_src = sapply(source[common], typeof)
  dt_tgt = sapply(target[common], typeof)

  #Create a data frame of dt_src and dt_tgt where they do not equal
  df = as.data.frame(cbind(dt_tgt, dt_src))[which(dt_src != dt_tgt),]
  names(df) = c(src_name, tgt_name)

  return(df)

}
