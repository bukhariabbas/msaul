# paste.from.excel function
# Michael C. Saul
# msaul [at] illinois.edu
# Started: 2015-06-26
# Last edited: 2016-06-26
#
#' Paste data frame from Excel
#' 
#' For a data frame copied from Excel, this function will paste it into the R environment.
#' In the current implementation, this function only works on a Mac.
#' 
#' @author Michael C. Saul
#' @param header whether your table has a header (defaults to TRUE)
#' @param stringsAsFactors whether character vectors should be converted to factors (defaults to FALSE)
#' @param representation test for overrepresentation or underrepresentation (defaults to "over")
#' @export

paste.from.excel = function(header = TRUE, stringsAsFactors = FALSE) {
  return(read.table(file = pipe("pbpaste"), 
                    sep = "\t", 
                    quote = "", 
                    header = header,
                    stringsAsFactors = stringsAsFactors))
}
