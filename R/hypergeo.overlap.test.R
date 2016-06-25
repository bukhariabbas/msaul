# author: Michael C. Saul
# email: msaul [at] illinois.edu
# project_start: 2015-04-29
# last_edited: 2016-06-09
#
#' Hypergeometric test of overlap function
#' 
#' For four character vectors (list1, list2, background1, background2), this function computes a hypergeometric p-value of the overlap.
#' It can test for either overrepresentation or underrepresention.
#' 
#' @param list1 the first hypergeometric test list
#' @param list2 the second hypergeometric test list
#' @param background1 the background for the first list
#' @param background2 the background for the second list
#' @param representation Test for overrepresentation or underrepresentation? Defaults to "over".
#' @export
#' @examples
#' x = c("one", "two", "three", "four", "five")
#' y = c("two", "three", "four", "five", "six")
#' x.bg = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven")
#' y.bg = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "twelve")
#' # should return p = 0.003968
#' hypergeo.overlap.test(x, y, x.bg, y.bg, representation = "over")

hypergeo.overlap.test = function(list1, list2, background1, background2, representation = "over") {
  # Requiring all arguments to the function to be character vectors.
  if (mode(list1) != "character"             |
            mode(list2) != "character"       |
            mode(background1) != "character" |
            mode(background2) != "character" |
            !is.vector(list1)                |
            !is.vector(list2)                |
            !is.vector(background1)          |
            !is.vector(background2)) {
    stop("All inputs must be character vectors.")
  }
  if (representation != "over" & representation != "under") {
    stop(paste('Argument for over- or under-representation test must be "over" or "under". Your value was "', 
               representation, 
               '".',
               sep = ""))
  }
  # Removing duplicate entries
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
  # This set of conditions is built to resolve an issue brought up by Joe Troy.
  # When the test resolves a universe to not contain a list of interest, the overrepresentation test
  # was coming up with a p-value of 0. This is not right, but I didn't want to throw an error and break
  # the function in loops. Thus, I revised to throw a warning and report the p-value as NA.
  if (len.list1.in.universe == 0) {
    warning("No item from list1 appears in the universe. A p-value is not calculable.")
    hypergeom.p.value = NA
  } else if (len.list2.in.universe == 0) {
    warning("No item from list2 appears in the universe. A p-value is not calculable.")
    hypergeom.p.value = NA
  } else {
    hypergeom.p.value = phyper(q = len.list1.list2.in.universe.overlap,
                               m = len.list1.in.universe, 
                               n = len.universe - len.list1.in.universe,
                               k = len.list2.in.universe,
                               lower.tail = ifelse(representation == "under", TRUE, FALSE))
  }
  test.list = list(p.value = hypergeom.p.value,
                   alternative = ifelse(representation == "under","less","greater"),
                   method = ifelse(representation == "under",
                                   "Hypergeometric Test for List Underrepresentation",
                                   "Hypergeometric Test for List Overrepresentation"),
                   data.name = paste("universe = ", len.universe,
                                     ", list 1 = ", len.list1.in.universe,
                                     ", list 2 = ", len.list2.in.universe,
                                     ", overlap of lists 1 and 2 = ", len.list1.list2.in.universe.overlap,
                                     sep = ""),
                   statistic = c(q = len.list1.list2.in.universe.overlap,
                                 m =len.list1.in.universe,
                                 n = len.universe - len.list1.in.universe,
                                 k = len.list2.in.universe))
  attr(test.list,"class") = "htest"
  return(test.list)
}