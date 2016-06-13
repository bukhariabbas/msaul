# orthoverlap test function
# Michael C. Saul
# msaul [at] illinois.edu
# Started: 2015-04-29
# Last edited: 2016-06-13
#
#' Orthogroup test for over- or underrepresentation
#' 
#' For four character vectors (list1.s1, list2.s2, orthodb.index.s1, orthodb.index.s2), this function computes a randomization-based p-value of the overlap.
#' It can test for either overrepresentation or underrepresention.
#' @author Michael C. Saul
#' @param list1.s1 the first test list from species 1
#' @param list2.s2 the second test list from species 2
#' @param orthodb.index.s1 a data frame indexing IDs from list 1 to OrthoDB IDs
#' @param orthodb.index.s2 a data frame indexing IDs from list 2 to OrthoDB IDs
#' @param representation Test for overrepresentation or underrepresentation? Defaults to "over".
#' @export

orthoverlap.test = function(list1.s1, list2.s2, orthodb.index.s1, orthodb.index.s2, B = 30000, representation = "over") {
  
  # Requiring all arguments to the function to be character vectors.
  if(mode(list1.s1) != "character"       |
     mode(list2.s2) != "character"       |
     !is.vector(list1)                   |
     !is.vector(list2)                   |
     !is.vector(background1.s1)          |
     !is.vector(background2.s2)) {
    stop("All input lists must be character vectors.")
  }
  if (representation != "over" & representation != "under") {
    stop(paste('Argument for over- or under-representation test must be "over" or "under". Your value was "',
               representation,
               '".',
               sep = ""))
  }
  
  #
  list1 = unique(list1.s1)
  list2 = unique(list2.s2)
  len.list1 = length(list1)
  len.list2 = length(list2)
  
  # For now, the OrthoDB indices are data frames. Row names are gene IDs for that species.
  # The column names are orthodb and id.
  # These are used to randomize orthology.
  # In the next step, the amount of homologs in each orthogroup of each species are calculated
  if (!is.data.frame(orthodb.index.s1) |
      !is.data.frame(orthodb.index.s2)) {
    stop("OrthoDB indices are malformed.")
  }
  
  orthodb.set = overlap(as.character(orthodb.index.s1$orthodb),as.character(orthodb.index.s2$orthodb))
  orthodb.set = data.frame(row.names=orthodb.set,
                           score.s1 = rep(0.0, times = length(orthodb.set)),
                           score.s2 = rep(0.0, times = length(orthodb.set)))
  
  #
  odb.n.s1 = aggregate(id ~ orthodb, data = orthodb.index.s1, FUN = length)
  row.names(odb.n.s1) = odb.n.s1$orthodb
  odb.n.s2 = aggregate(id ~ orthodb, data = orthodb.index.s2, FUN = length)
  row.names(odb.n.s2) = odb.n.s2$orthodb
  
  #
  list1.odb = data.frame(id = as.character(list1))
  list1.odb$orthodb = as.character(orthodb.index.s1[as.character(list1.odb$id),"orthodb"])
  list1.odb = list1.odb[!is.na(list1.odb$orthodb),]
  list1.n = aggregate(id ~ orthodb, data = list1.odb, FUN = length)
  row.names(list1.n) = list1.n$orthodb
  list1.genes.in.orthodb = sum(list1.n$id)
  list1.n$bg.sum = odb.n.s1[list1.n$orthodb,"id"]
  # Trying a new score
  list1.n$score = list1.n$id * (sqrt(1 / list1.n$bg.sum))
  
  #
  list2.odb = data.frame(id = as.character(list2))
  list2.odb$orthodb = as.character(orthodb.index.s2[as.character(list2.odb$id),"orthodb"])
  list2.odb = list2.odb[!is.na(list2.odb$orthodb),]
  list2.n = aggregate(id ~ orthodb, data = list2.odb, FUN = length)
  row.names(list2.n) = list2.n$orthodb
  list2.genes.in.orthodb = sum(list2.n$id)
  list2.n$bg.sum = odb.n.s2[list2.n$orthodb,"id"]
  list2.n$score = list2.n$id * (sqrt(1/ list2.n$bg.sum))
  
  # So I don't know if this part is proper to do.
  # I'm filtering to say you can get a score only if you're a double hit.
  # double.hits = # overlap(as.character(list1.n$orthodb),as.character(list2.n$orthodb))
  orthodb.set[row.names(list1.n),"score.s1"] = list1.n[row.names(list1.n),"score"]
  orthodb.set[row.names(list2.n),"score.s2"] = list2.n[row.names(list2.n),"score"]
  
  # 
  orthodb.set.double.hit = which(orthodb.set$score.s1 != 0 & orthodb.set$score.s2 != 0)
  empirical.score = sum(orthodb.set[orthodb.set.double.hit,"score.s1"] * orthodb.set[orthodb.set.double.hit,"score.s2"])

  #
  greater.than.histogram = B
  
  #
  for (i in 1:B) {
    random.list = data.frame(x1 = sample(orthodb.set$score.s1),
                             x2 = sample(orthodb.set$score.s2))
    random.double.hit = which(random.list$x1 != 0 & random.list$x2 != 0)
    random.score = sum(random.list[random.double.hit,"x1"] * random.list[random.double.hit,"x2"])
    if (random.score < empirical.score) {
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
                                   "Orthology-Weighted Randomization-Based Test for Underrepresentation",
                                   "Orthology-Weighted Randomization-Based Test for Overrepresentation"),
                   data.name = paste("list 1 = ", len.list1,
                                     ", list 2 = ", len.list2,
                                     ", empirical score = ", round(empirical.score, digits = 2),
                                     sep = ""),
                   statistic = c(B = B,
                                 gtr.than.E = greater.than.histogram))
  attr(test.list,"class") = "htest"
  return(test.list)
}
