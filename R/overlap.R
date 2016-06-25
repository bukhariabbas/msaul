# author: Michael C. Saul
# email: msaul [at] illinois.edu
# project_start: 2015-04-29
# last_edited: 2016-01-07
#
#' Overlap function
#' 
#' This function finds the overlap between two character vectors
#' @author Michael C. Saul
#' @param vector1 the first character vector
#' @param vector2 the second character vector
#' @export
#' @examples
#' x = c("one", "two", "three", "four", "five")
#' y = c("two", "four", "six", "eight", "ten")
#' # should return c("two", "four")
#' overlap(x,y)

overlap = function(vector1, vector2) {
  # Requiring all arguments to the function to be character vectors.
  if (mode(vector1) != "character" | 
      mode(vector2) != "character" |
      !is.vector(vector1)          |
      !is.vector(vector2)) {
    stop("All inputs must be character vectors.")
  }
  return(unique(vector1[which(vector1 %in% vector2)]))
}