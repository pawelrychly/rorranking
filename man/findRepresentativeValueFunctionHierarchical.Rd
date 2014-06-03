\name{findRepresentativeValueFunctionHierarchical}
\alias{findRepresentativeValueFunctionHierarchical}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
findRepresentativeValueFunctionHierarchical
}
\description{
Function finds representative value function for each node of hierarchy in defined problem
}
\usage{
findRepresentativeValueFunctionHierarchical(perf, strict.vf = FALSE, strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL, strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, rank.related.requirements = NULL, nums.of.characteristic.points = NULL, nec.relations = NULL, is.compromise = FALSE, k = 0, hierarchy.data = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{perf}{
  m x n performance matrix with m alternatives and n criteria}
  \item{strict.vf}{
  Whether to use sctrictly increasing (TRUE) or monotonously increasing (FALSE) value functions.}
 \item{strong.prefs}{
  k x 2 or k x 3 matrix of strong preferences statements (row indices of alternatives in the performance matrix). Each row r is a preference statements meaning that preferences[k,1] is strongly preferred to preferences[k,2]. The third column is optional and it describes a node id, which describes a criteria set, which should be considered. }
  \item{weak.prefs}{
  k x 2 or k x 3 matrix of strong preferences statements (row indices of alternatives in the performance matrix). Each row r is a preference statements meaning that preferences[k,1] is strongly preferred to preferences[k,2]. The third column is optional and it describes a node id, which describes a criteria set, which should be considered. }
  \item{indif.prefs}{
  k x 2 or k x 3 matrix of indifference preferences statements (row indices of alternatives in the performance matrix). Each row r is a preference statements meaning that preferences[k,1] is equally preferred to preferences[k,2]. The third column is optional and it describes a node id, which describes a criteria set, which should be considered. }
  \item{strong.intensities.of.prefs}{
    k x 4 or k x 5 matrix of strong preference intensities of preferences statements. Each row r mean that intensity preference relation between [k,1] and [k,2] is strongly preferred to intensity of preference relation between [k,3] and [k,4]. Fifth column is optional and could be used to determine a criterion which should be considered.
  }
  \item{weak.intensities.of.prefs}{
  k x 4 or k x 5 matrix of weak preference intensities of preferences statements. Each row r mean that intensity preference relation between [k,1] and [k,2] is weakly preferred to intensity of preference relation between [k,3] and [k,4]. Fifth column is optional and could be used to determine a criterion which should be considered.
  }
  \item{indif.intensities.of.prefs}{
  k x 4 or k x 5 matrix of indifferences between intensities of preference statements. Each row r mean that intensity preference relation between [k,1] and [k,2] is indifferent to intensity of preference relation between [k,3] and [k,4]. Fifth column is optional and could be used to determine a criterion which should be considered.
  }
  \item{rank.related.requirements}{
  k x 3 or k x 4 matrix. Each row means that alternative with id [k, 1] in the final ranking should achive a position between [k, 2] and [k, 3]. Fourth column is optional and could be used to determine a criterion which should be considered.
}
  \item{nums.of.characteristic.points}{ List of integers. value on n-th position, describes number of characteristic points used in n-th criterion. If value is 0, then no characteristic points are used. }
  \item{nec.relations}{
    A list keyed by node ids (described in hierarchy.data). Each value of list is a m x 2  matrix with m alternatives. First column describes the worst possible ranking. The second column describes the best possible ranking position of variant.
  }
  \item{is.compromise}{
if TRUE, then a compromise method of finding function is used. 
}
  \item{k}{
 The non-negative relaxation threshold 
}
   \item{hierarchy.data}{
    a list which contains two elements. 
    criteria.by.nodes - Is a list of criteria ids list, keyed by an node ids.
    hierarchy.tree - describes a hierarchy. It is a recursive list, keyed by a node ids. example: list(root = list(a1=list(a11=list(), a12=list()), a2=list()))

  }
}

\value{
A list indexed by a hierarchy node ids. Each value is a n x 2 marix. Each row determines a point of representative value function.
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (perf, strict.vf = FALSE, strong.prefs = NULL, weak.prefs = NULL, 
    indif.prefs = NULL, strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, 
    indif.intensities.of.prefs = NULL, rank.related.requirements = NULL, 
    nums.of.characteristic.points = NULL, nec.relations = NULL, 
    is.compromise = FALSE, k = 0, hierarchy.data = NULL) 
{
    results <- list()
    hierarchy.data <- prepareHierarchyData(perf = perf, hierarchy.data = hierarchy.data)
    err <- NULL
    if (hierarchy.data[["status"]] == "OK") {
        nodes <- hierarchy.data$nodes
        criteria.by.nodes <- hierarchy.data$criteria.by.nodes
        for (node.id in nodes) {
            func <- list()
            if (is.compromise) {
                func <- findRepresentativeValueFunctionCompromise(perf, 
                  strict.vf = strict.vf, strong.prefs = strong.prefs, 
                  weak.prefs = weak.prefs, indif.prefs = indif.prefs, 
                  strong.intensities.of.prefs = strong.intensities.of.prefs, 
                  weak.intensities.of.prefs = weak.intensities.of.prefs, 
                  indif.intensities.of.prefs = indif.intensities.of.prefs, 
                  rank.related.requirements = rank.related.requirements, 
                  nums.of.characteristic.points = nums.of.characteristic.points, 
                  nec.relations.matrix = nec.relations[[node.id]], 
                  criteria.by.nodes = criteria.by.nodes, nodeid = node.id)
            }
            else {
                func <- findRepresentativeValueFunctionIterative(perf, 
                  strict.vf = strict.vf, strong.prefs = strong.prefs, 
                  weak.prefs = weak.prefs, indif.prefs = indif.prefs, 
                  strong.intensities.of.prefs = strong.intensities.of.prefs, 
                  weak.intensities.of.prefs = weak.intensities.of.prefs, 
                  indif.intensities.of.prefs = indif.intensities.of.prefs, 
                  rank.related.requirements = rank.related.requirements, 
                  nums.of.characteristic.points = nums.of.characteristic.points, 
                  nec.relations.matrix = nec.relations[[node.id]], 
                  k = k, criteria.by.nodes = criteria.by.nodes, 
                  nodeid = node.id)
            }
            results[[node.id]] = func
        }
    }
    return(results)
  }
}
