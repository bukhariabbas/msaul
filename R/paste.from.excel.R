# paste.from.excel function
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
