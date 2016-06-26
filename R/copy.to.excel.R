# copy.to.excel function
# Michael C. Saul
# msaul [at] illinois.edu
# Started: 2015-06-26
# Last edited: 2016-06-26
#
#' Copy a data frame to Excel
#' 
#' For a data frame in your R environment, this function will copy it to the clipboard in a format that can be pasted into Excel.
#' In the current implementation, this function only works on a Mac.
#' 
#' @author Michael C. Saul
#' @param row.names whether your output table should include row names (defaults to FALSE)
#' @param col.names whether your output table should include column names (defaults to TRUE)
#' @param quote whether your output should include quotation marks as field delimiters (defaults to FALSE)
#' @export

copy.to.excel = function(x, row.names = FALSE, col.names = TRUE, quote = FALSE) {
  return(write.table(x = x,
                     file = pipe("pbcopy"), 
                     sep = "\t", 
                     quote = FALSE, 
                     row.names = row.names,
                     col.names = col.names))
}
