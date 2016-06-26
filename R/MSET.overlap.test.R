# MSET test function
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
# Started: 2015-04-29
# Last edited: 2016-06-25
#
#' Modular single-set enrichment tool (MSET): randomization-based test for list over- or under-representation
#'
#' MSET was developed to compare gene lists.
#' For four character vectors (list1, list2, background1, background2), this function computes a randomization-based p-value of the overlap.
#' It can test for either overrepresentation or underrepresention.
#' MSET is based on work from Eisinger et al., 2013, "Development of a versatile enrichment analysis tool reveals associations between the maternal brain and mental health disorders, including autism." BMC Neuroscience.
#' 
#' @param list1 the first test list
#' @param list2 the second test list
#' @param background1 the background for the first list
#' @param background2 the background for the second list
#' @param representation Test for overrepresentation or underrepresentation? Defaults to "over".
#' @export
#' @examples
#' x = c("one", "two", "three", "four", "five")
#' y = c("two", "four", "six", "eight", "ten")
#' x.bg = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven")
#' y.bg = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "twelve")
#' MSET.overlap.test(x, y, x.bg, y.bg)

MSET.overlap.test = function(list1, list2, background1, background2, B = 30000, representation = "over") {
  # Requiring all arguments to the function to be character vectors.
  if(mode(list1) != "character"         |
       mode(list2) != "character"       |
       mode(background1) != "character" |
       !is.vector(list1)                |
       !is.vector(list2)                |
       !is.vector(background1)) {
    stop("All inputs must be character vectors.")
  }
  if (representation != "over" & representation != "under") {
    stop(paste('Argument for over- or under-representation test must be "over" or "under". Your value was "', 
               representation, 
               '".',
               sep = ""))
  }
  list1 = unique(list1)
  list2 = unique(list2)
  background1 = unique(background1)
  background2 = unique(background2)
  # Making sure that the lists are included in their backgrounds
  if (length(list1) != length(overlap(list1, background1)) |
        length(list2) != length(overlap(list2, background2))) {
    stop("Lists of interest must be included in backgrounds.")
  }
  len.background1 = length(background1)
  len.background2 = length(background2)
  universe = overlap(background1, background2)
  len.universe = length(universe)
  list1.in.universe = overlap(list1, universe)
  list2.in.universe = overlap(list2, universe)
  len.list1.in.universe = length(list1.in.universe)
  len.list2.in.universe = length(list2.in.universe)
  list1.list2.in.universe.overlap = overlap(list1.in.universe, list2.in.universe)
  len.list1.list2.in.universe.overlap = length(list1.list2.in.universe.overlap)
  greater.than.histogram = B
  for (i in 1:B) {
    rand.list1 = sample(universe, len.list1.in.universe, replace = F)
    rand.list2 = sample(universe, len.list2.in.universe, replace = F)
    if (length(overlap(rand.list1, rand.list2)) < len.list1.list2.in.universe.overlap) {
      greater.than.histogram = greater.than.histogram - 1
    } else {
      next
    }
  }
  if (representation == "over") {
    mset.p.value = greater.than.histogram / B
  } else if (representation == "under") {
    mset.p.value = (B - greater.than.histogram) / B
  } else {
    stop(paste('Argument for over- or under-representation test must be "over" or "under". Your value was "', 
               representation, 
               '".',
               sep = ""))
  }
  test.list = list(p.value = mset.p.value,
                   alternative = ifelse(representation == "under",
                                        "less",
                                        "greater"),
                   method = ifelse(representation == "under",
                                   "MSET Randomization-Based Test for Underrepresentation",
                                   "MSET Randomization-Based Test for Overrepresentation"),
                   data.name = paste("universe = ", len.universe,
                                     ", list 1 = ", len.list1.in.universe,
                                     ", list 2 = ", len.list2.in.universe,
                                     ", overlap of lists 1 and 2 = ", len.list1.list2.in.universe.overlap,
                                     sep = ""),
                   statistic = c(B = B,
                                 gtr.than.E = greater.than.histogram))
  attr(test.list,"class") = "htest"
  return(test.list)
}