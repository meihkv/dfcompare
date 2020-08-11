#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

df_compare = function (source, target, keys) {
  require(dplyr)
  require(magrittr)
  {
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
    src_name = deparse(substitute(source))
    tgt_name = deparse(substitute(target))
    #Test cases
    #No matching columns
    #No rows
    #No matching keys

  }
}

# print(sprintf("Columns in %s not in %s:",src_name, tgt_name))
# print(mismatch_colnames(source, target)[1])
# print(sprintf("Columns in %s not in %s:",tgt_name, src_name))
# print(mismatch_colnames(source, target)[2])
#
# print("Mismatching datatypes:")
# print(mismatch_datatypes(source, target))
