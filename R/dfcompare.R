#' Compare two data frames
#'
#' This function compares two data frames.  It checks if the source and target
#' variables are data frames, and if there are rows in both,
#' and if the keys are a character vector, and that they keys
#' exist in both source and target data frames.
#'
#' @param source data frame of the source
#' @param target data frame of the target
#' @param keys a string containing a key. Pass a character vector for multiple keys. The keys will be used in a data.table join using on =.
#' @return a list of data.tables for each column and the corresponding mismatches
#' @export
dfcompare = function (source, target, keys, summary = TRUE) {

  stopifnot(
    is.data.frame(source),
    is.data.frame(target),
    nrow(source) > 1,
    nrow(target) > 1,
    is.vector(keys),
    is.character(keys),
    col_exists(source, keys),
    col_exists(target, keys)
  )

  #prepare keys
  names(keys) = keys

  #Get actual names of source and target
  src_name = deparse(substitute(source))
  tgt_name = deparse(substitute(target))

  #Convert to data table
  if (!is.data.table(source)) {
    setDT(source)
  }

  if(!is.data.table(target)) {
    setDT(target)
  }

  #Get a vector of common column names
  common = common_colnames(source,target)

  #Get common column names without keys
  common_nokey = sort(common[!(common %in% keys)])

  if(length(common_nokey)==0){
    stop("There are no common columns to compare.")
  }

  #Get a list of mismatching column names and convert to a data frame
  mismatch_colnames= mismatch_colnames(source, target, src_name, tgt_name)

  #Get a data frame of mismatching data types
  mismatch_datatypes= mismatch_datatypes(source, target, src_name, tgt_name)

  #Convert data frames to data tables for performance
  source = data.table::as.data.table(source)[,..common]
  target = data.table::as.data.table(target)[,..common]

  #Label columns using actual source and target names
  # names(mismatch_colnames) = c(paste(src_name, "not in", tgt_name, sep=" "),
  #                              paste(tgt_name, "not in", src_name, sep=" "))

  #Remove duplicate keys from source and target
  src_no_dupes = source[!(duplicated(source[,..keys]) |
                            duplicated(source[,..keys], fromLast = TRUE)),]
  src_dupes = source[(duplicated(source[,..keys]) |
                         duplicated(source[,..keys], fromLast = TRUE)),]
  tgt_no_dupes = target[!(duplicated(target[,..keys]) |
                            duplicated(target[,..keys], fromLast = TRUE)),]
  tgt_dupes = target[(duplicated(target[,..keys]) |
                         duplicated(target[,..keys], fromLast = TRUE)),]

  #Merge non-duplicated source and target on keys to compare values
  src_and_tgt = src_no_dupes[tgt_no_dupes, on = keys, nomatch = 0]

  #Checks each column pairs between source and target for equal values
  list = lapply(common_nokey, datatable = src_and_tgt, keys = keys, dt_names = c(src_name, tgt_name), check_equality)
  names(list) = common_nokey

  #Get mismatch counts for each data frame contained in list
  printout = lapply(list,nrow)
  #Create a data table of mismatch counts
  printout= data.table::data.table(column = unlist(names(printout)),
                  mismatches = unlist(printout))
  #Remove named rows for cleanliness
  names(printout) = c('Column','Mismatches')

  #Summary printout
  if(summary){
    cat("--DF Compare--\n")
    cat("\n")

    cat("Data Frame Summary\n")
    cat("---------------------------\n")
    cat("Key(s) used:",keys,"\n")
    cat("Observations in",src_name,": ",nrow(source),"\n")
    cat("Observations in",tgt_name,": ",nrow(target),"\n")
    cat("Duplicate keys removed from",src_name,":",nrow(src_dupes),"\n")
    cat("Duplicate keys removed from",tgt_name,":",nrow(tgt_dupes),"\n")
    cat("Observations compared:",nrow(src_and_tgt),"\n")
    cat("Columns compared:",length(common_nokey),"\n")
    cat("Columns with unequal values:",nrow(printout[,printout[Mismatches>0]]),"\n")
    cat("\n")
    cat("Uncommon column names:\n")
    cat("---------------------------\n")
    if(nrow(mismatch_colnames[[1]])>0 | nrow(mismatch_colnames[[2]])>0) {

      if(nrow(mismatch_colnames[[1]])>0) {
        print(mismatch_colnames[[1]], row.names = FALSE)
      }
      if(nrow(mismatch_colnames[[2]])>0) {
        print(mismatch_colnames[[2]], row.names = FALSE)
      }
    }
    else{
      cat("None\n")
    }
    cat("\n")
    cat("Mismatching datatypes:\n")
    cat("---------------------------\n")
    if (nrow(mismatch_datatypes)>0) {
      print(mismatch_datatypes)
    }
    else{
      cat("None\n")
    }
    cat("\n")
    cat("\n")
    cat("Mismatching values:\n")
    cat("---------------------------\n")

    print(printout)
    cat("\n")
    cat("\n")
  }

  return(list)
}
