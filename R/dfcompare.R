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
dfcompare = function (source, target, keys) {

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

  print("Mismatching column names")
  print(mismatch_colnames(source, target))
  print("Mismatching datatypes:")
  print(mismatch_datatypes(source, target))

  common = common_colnames(source,target)

  source = data.table::as.data.table(source)[,..common]
  target = data.table::as.data.table(target)[,..common]

  #Remove duplicate keys from source and target
  src_no_dupes = source[!(duplicated(data.table::rleidv(source, cols = keys)) |
                            duplicated(data.table::rleidv(source, cols = keys), fromLast = TRUE)),]
  src_dupes = source[(duplicated(data.table::rleidv(source, cols = keys)) |
                         duplicated(data.table::rleidv(source, cols = keys), fromLast = TRUE)),]
  tgt_no_dupes = target[!(duplicated(data.table::rleidv(target, cols = keys)) |
                            duplicated(data.table::rleidv(target, cols = keys), fromLast = TRUE)),]
  tgt_dupes = target[(duplicated(data.table::rleidv(target, cols = keys)) |
                         duplicated(data.table::rleidv(target, cols = keys), fromLast = TRUE)),]

  #prepare keys
  names(keys) = keys

  #merge data tables on keys
  src_and_tgt = src_no_dupes[tgt_no_dupes, on = keys, nomatch = 0]

  #Extracts column names without keys
  common_nokey = sort(common[!(common %in% keys)])

  #Checks each column pairs between source and target for equality
  list = lapply(common_nokey, datatable = src_and_tgt, check_equality)
  names(list) = common_nokey

  return(list)
}