# copy.to.excel function
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
