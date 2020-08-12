\name{dfcompare}
\alias{dfcompare}
\title{dfcompare}
\usage{
df_compare(source_test, target_test, keys=c('key1','key2','key3'))
}
\description{
Runs dfcompare
}
\examples{

key1 = c(1,2,3,4,5,6,6,6,9)
key2 = c(1,2,3,4,5,2,6,6,9)
key3 = c(1,2,3,4,5,6,6,6,9)
col1= c('a','b','c','d','y','f','g','h','i')
col4= c('a','b','b','d','e','f','g','h','i')
col2= c('a','b','b','d','e','z','g','h','z')

source_test = as.data.frame(cbind(key1,col2,col4,key2,key3,col1))

source_test$col1 = as.character(source_test$col1)

key1 = c(11,2,3,4,5,6,6,12,9)
key2 = c(1,2,3,4,5,6,3,7,9)
key3 = c(1,2,3,4,5,6,6,7,9)
col1= c('a','b','c','d','e','f','g','h','i')
col2= c('a','1','b','d','e','f','g','h','i')
col3= c('a','b','b','d','e','f','g','h','i')
col5= c('a','b','b','d','e','f','g','h','i')

target_test = as.data.frame(cbind(key1,key2,key3,col1,col2,col3,col5))

target_test$col1 = as.character(target_test$col1)
target_test$col2 = as.character(target_test$col2)
target_test$col3 = as.character(target_test$col3)


#
# setDT(source_test)
# setDT(target_test)
#
# src_no_dupes = source_test[!duplicated(rleidv(source_test, cols = keys)), ]
# src_dupes = source_test[duplicated(rleidv(source_test, cols = keys)), ]
# tgt_no_dupes = target_test[!duplicated(rleidv(target_test, cols = keys)), ]
# tgt_dupes = target_test[duplicated(rleidv(target_test, cols = keys)), ]
#
# src_and_tgt = src_no_dupes[tgt_no_dupes, on = keys, nomatch = 0]

merged = df_compare(source_test, target_test, keys=c('key1','key2','key3'))
}
