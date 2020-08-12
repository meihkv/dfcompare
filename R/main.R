
#Test cases
#No matching columns
#No rows
#No matching keys

# print(sprintf("Columns in %s not in %s:",src_name, tgt_name))
# print(mismatch_colnames(source, target)[1])
# print(sprintf("Columns in %s not in %s:",tgt_name, src_name))
# print(mismatch_colnames(source, target)[2])
#
# print("Mismatching datatypes:")
# print(mismatch_datatypes(source, target))

df_compare = function (source, target, keys) {

  stopifnot(
    is.data.frame(source),
    is.data.frame(target),
    nrow(source) > 1,
    nrow(target) > 1,
    is.vector(keys),
    is.character(keys),
    key_exists(source, keys),
    key_exists(target, keys)
  )

  print("Mismatching column names")
  print(mismatch_colnames(source, target))
  print("Mismatching datatypes:")
  print(mismatch_datatypes(source, target))

  common = common_colnames(source,target)

  source = as.data.table(source)[,..common]
  target = as.data.table(target)[,..common]

  #Remove duplicate keys from source and target
  src_no_dupes = source[!duplicated(rleidv(source, cols = keys)), ]
  src_dupes = source[duplicated(rleidv(source, cols = keys)), ]
  tgt_no_dupes = target[!duplicated(rleidv(target, cols = keys)), ]
  tgt_dupes = target[duplicated(rleidv(target, cols = keys)), ]

  #prepare keys
  names(keys) = keys

  #merge data tables on keys
  src_and_tgt = src_no_dupes[tgt_no_dupes, on = keys, nomatch = 0]

  return(src_and_tgt)

}
