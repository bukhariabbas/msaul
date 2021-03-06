# orthoverlap test function
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
# Last edited: 2016-10-31
#
#' Orthogroup test for over- or underrepresentation
#'
#' For four character vectors (list1.s1, list2.s2, orthodb.index.s1, orthodb.index.s2), this function computes a randomization-based p-value of the overlap.
#' It can test for either overrepresentation or underrepresention.
#' Orthoverlap uses a normal approximation of the hypergeometric distribution to make scores from different distributions directly comparable.
#' It calculates the p-value by comparing the Pearson correlation coefficient derived from data with orthology to a sample with orthology bootstrapped.
#'
#' @author Michael C. Saul
#' @param list1.s1 the first test list from species 1
#' @param list2.s2 the second test list from species 2
#' @param orthodb.index.s1 a data frame indexing IDs from list 1 to OrthoDB IDs
#' @param orthodb.index.s2 a data frame indexing IDs from list 2 to OrthoDB IDs
#' @param B the number of randomizations to perform (defaults to 30000)
#' @param save.distribution write out distribution to output (defaults to FALSE)
#' @param representation test for overrepresentation or underrepresentation (defaults to "over")
#' @export

orthoverlap.test = function(list1.s1, list2.s2, orthodb.index.s1, orthodb.index.s2, B = 30000, representation = "over", save.distribution = FALSE) {
  
  # Requiring all arguments to the function to be character vectors.
  if(mode(list1.s1) != "character"       |
     mode(list2.s2) != "character") {
    stop("All input lists must be character vectors.")
  }
  if (representation != "over" & representation != "under") {
    stop(paste('Argument for over- or under-representation test must be "over" or "under". Your value was "',
               representation,
               '".',
               sep = ""))
  }
  
  # Generating unique lists from each species.
  list1 = unique(list1.s1)
  list2 = unique(list2.s2)
  len.list1 = length(list1)
  len.list2 = length(list2)
  
  # OrthoDB indices are data frames. Row names are gene IDs for that species.
  # The column names are orthodb and id.
  # These are used to calculate and randomize orthology.
  # In the next step, the amount of homologs in each orthogroup of each species are calculated
  if (!is.data.frame(orthodb.index.s1) |
      !is.data.frame(orthodb.index.s2)) {
    stop("OrthoDB indices are malformed.")
  }
  
  # Generating a data frame to hold the orthogroup information
  orthodb.set = overlap(as.character(orthodb.index.s1$orthodb),as.character(orthodb.index.s2$orthodb))
  orthodb.set = data.frame(row.names=orthodb.set,
                           size.s1 = rep(0, times = length(orthodb.set)),
                           hits.s1 = rep(0, times = length(orthodb.set)),
                           numerator.s1 = rep(0.0, times = length(orthodb.set)),
                           denominator.s1 = rep(0.0, times = length(orthodb.set)),
                           score.s1 = rep(0.0, times = length(orthodb.set)),
                           hypergeo.p.s1 = rep(0.0, times = length(orthodb.set)),
                           size.s2 = rep(0, times = length(orthodb.set)),
                           hits.s2 = rep(0, times = length(orthodb.set)),
                           numerator.s2 = rep(0.0, times = length(orthodb.set)),
                           denominator.s2 = rep(0.0, times = length(orthodb.set)),
                           score.s2 = rep(0.0, times = length(orthodb.set)),
                           hypergeo.p.s2 = rep(0.0, times = length(orthodb.set)))
  
  # Getting counts of total genes in each orthogroup
  odb.n.s1 = aggregate(id ~ orthodb, data = orthodb.index.s1, FUN = length)
  row.names(odb.n.s1) = odb.n.s1$orthodb
  orthodb.set[,"size.s1"] = odb.n.s1[row.names(orthodb.set),"id"]
  odb.n.s2 = aggregate(id ~ orthodb, data = orthodb.index.s2, FUN = length)
  row.names(odb.n.s2) = odb.n.s2$orthodb
  orthodb.set[,"size.s2"] = odb.n.s2[row.names(orthodb.set),"id"]
  
  # Getting counts of genes in each orthogroup in list1.s1
  list1.odb = data.frame(id = as.character(list1))
  list1.odb$orthodb = as.character(orthodb.index.s1[as.character(list1.odb$id),"orthodb"])
  list1.odb = list1.odb[!is.na(list1.odb$orthodb),]
  list1.n = aggregate(id ~ orthodb, data = list1.odb, FUN = length)
  row.names(list1.n) = list1.n$orthodb
  list1.genes.in.orthodb = sum(list1.n$id)
  list1.n$bg.sum = odb.n.s1[list1.n$orthodb,"id"]
  list1.p = sum(list1.n$id) / nrow(orthodb.index.s1)
  orthodb.set[row.names(list1.n),"hits.s1"] = list1.n$id
  total.s1 = sum(orthodb.set$size.s1)
  hits.s1 = sum(orthodb.set$hits.s1)
  
  # Getting counts of genes in each orthogroup in list2.s2
  list2.odb = data.frame(id = as.character(list2))
  list2.odb$orthodb = as.character(orthodb.index.s2[as.character(list2.odb$id),"orthodb"])
  list2.odb = list2.odb[!is.na(list2.odb$orthodb),]
  list2.n = aggregate(id ~ orthodb, data = list2.odb, FUN = length)
  row.names(list2.n) = list2.n$orthodb
  list2.genes.in.orthodb = sum(list2.n$id)
  list2.n$bg.sum = odb.n.s2[list2.n$orthodb,"id"]
  list2.p = sum(list2.n$id) / nrow(orthodb.index.s2)
  orthodb.set[row.names(list2.n),"hits.s2"] = list2.n$id
  total.s2 = sum(orthodb.set$size.s2)
  hits.s2 = sum(orthodb.set$hits.s2)
  
  # Integrating information from each individual species in the orthoDB data frame into Z-Scores
  # Note: approximated from the hypergeometric distribution
  orthodb.set$numerator.s1 = orthodb.set$hits.s1 - ((orthodb.set$size.s1 * hits.s1) / total.s1)
  orthodb.set$denominator.s1 = sqrt((orthodb.set$size.s1 * total.s1 * (total.s1 - hits.s1) * (total.s1 - orthodb.set$size.s1)) / ((total.s1 ^ 2) * (total.s1 - 1)))
  orthodb.set$score.s1 = orthodb.set$numerator.s1 / orthodb.set$denominator.s1
  orthodb.set$numerator.s2 = orthodb.set$hits.s2 - ((orthodb.set$size.s2 * hits.s2) / total.s2)
  orthodb.set$denominator.s2 = sqrt((orthodb.set$size.s2 * total.s2 * (total.s2 - hits.s2) * (total.s2 - orthodb.set$size.s2)) / ((total.s2 ^ 2) * (total.s2 - 1)))
  orthodb.set$score.s2 = orthodb.set$numerator.s2 / orthodb.set$denominator.s2
  
  for (j in 1:nrow(orthodb.set)) {
    orthodb.set[j,"hypergeo.p.s1"] = phyper(q = orthodb.set[j,"hits.s1"],
                                            m = hits.s1,
                                            n = (total.s1 - hits.s1),
                                            k = orthodb.set[j,"size.s1"],
                                            lower.tail = FALSE)
    orthodb.set[j,"hypergeo.p.s2"] = phyper(q = orthodb.set[j,"hits.s2"],
                                            m = hits.s2,
                                            n = (total.s2 - hits.s2),
                                            k = orthodb.set[j,"size.s2"],
                                            lower.tail = FALSE)
  }
  
  orthodb.set$individual.score = orthodb.set$score.s1 * orthodb.set$score.s2
  
  # Calculating score
  empirical.score = cor(orthodb.set$score.s1, orthodb.set$score.s2, method = "pearson")
  
  # Creating a histogram for the calculation of p-values
  randomization.histogram = B
  
  # Creating an empty object for distribution
  if (save.distribution) {
    random.distribution = numeric()
  }
  
  # Randomizing over orthology (B iterations)
  for (i in 1:B) {
    random.list = data.frame(x1 = sample(orthodb.set$score.s1),
                             x2 = sample(orthodb.set$score.s2))
    random.score = cor(random.list$x1, random.list$x2, method = "pearson")
    if (save.distribution) {
      random.distribution = c(random.distribution, random.score)
    }
    if (representation == "over") {
      if (random.score < empirical.score) {
        randomization.histogram = randomization.histogram - 1
      } else {
        next
      }
    } else if (representation == "under") {
      if (random.score > empirical.score) {
        randomization.histogram = randomization.histogram - 1
      } else {
        next
      }
    } else {
      stop(paste('Argument for over- or under-representation test must be "over" or "under". Your value was "',
                 representation,
                 '".',
                 sep = ""))
    }
  }
  
  # Calculating p-value from histogram
  orthoverlap.p.value = randomization.histogram / B
  
  # Putting together hypothesis test object and returning it
  test.list = list(p.value = orthoverlap.p.value,
                   alternative = ifelse(representation == "under",
                                        "less",
                                        "greater"),
                   method = ifelse(representation == "under",
                                   "Orthology-Weighted Randomization-Based Test for Depletion",
                                   "Orthology-Weighted Randomization-Based Test for Enrichment"),
                   data.name = paste("list 1 = ", len.list1,
                                     ", list 2 = ", len.list2,
                                     ", empirical score = ", round(empirical.score, digits = 2),
                                     sep = ""),
                   statistic = c(B = B,
                                 more.extreme.than.empirical = randomization.histogram))
  if (save.distribution) {
    test.list[["distribution"]] = random.distribution
  }
  test.list[["orthodb.set"]] = orthodb.set
  test.list[["p.s1"]] = list1.p
  test.list[["p.s2"]] = list2.p
  test.list[["total.s1"]] = total.s1
  test.list[["total.s2"]] = total.s2
  test.list[["hits.s1"]] = hits.s1
  test.list[["hits.s2"]] = hits.s2
  attr(test.list,"class") = "htest"
  return(test.list)
}