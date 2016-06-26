# overlap function
# Copyright 2016, Michael C. Saul
# msaul [at] illinois.edu
# 
# This file is part of the msaul R package.
# 
# The msaul R package is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# The msaul R package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with the msaul R package  If not, see <http://www.gnu.org/licenses/>.
# 
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