\name{findRankAcceptabilityIndicesHierarchical}
\alias{findRankAcceptabilityIndicesHierarchical}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
findRankAcceptabilityIndicesHierarchical
}
\description{
Function finds a list of rank acceptability indices matrixes for a given problem. In this version of function, a hierarchical informations are considered. 
}
\usage{
findRankAcceptabilityIndicesHierarchical(perf, strict.vf, strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL, strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, rank.related.requirements = NULL, nums.of.characteristic.points = NULL, criteria=NULL, num.of.samples = 100, hierarchy.data = NULL)
}
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
  \item{criteria}{ List of characters. value on n-th position, determines the type of n-th criterion. If value equals "c" then criterion is cost. Otherwise the type of criterion is gain. }
  \item{num.of.samples}{
  number of sempled value function. default value is 100.
}
  \item{hierarchy.data}{
    a list which contains two elements. 
    criteria.by.nodes - Is a list of criteria ids list, keyed by an node ids.
    hierarchy.tree - describes a hierarchy. It is a recursive list, keyed by a node ids. example: list(root = list(a1=list(a11=list(), a12=list()), a2=list()))

  }
}

\value{
A list indexed by a hierarchy node ids. Each value is a m x m matrix where m is a number of alternatives. Each value of this matrix is a rank acceptability indice for a given alternative (rows) and rank position (columns).
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (perf, strict.vf, strong.prefs = NULL, weak.prefs = NULL, 
    indif.prefs = NULL, strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, 
    indif.intensities.of.prefs = NULL, rank.related.requirements = NULL, 
    nums.of.characteristic.points = NULL, num.of.samples = 100, 
    hierarchy.data = NULL) 
{
    results <- list()
    hierarchy.data <- prepareHierarchyData(perf, hierarchy.data)
    if (hierarchy.data[["status"]] == "OK") {
        nodes <- hierarchy.data$nodes
        criteria.by.nodes <- hierarchy.data$criteria.by.nodes
        for (node.id in nodes) {
            result <- findRankAcceptabilityIndicesHelper(perf = perf, 
                strict.vf = strict.vf, strong.prefs = strong.prefs, 
                weak.prefs = weak.prefs, indif.prefs = indif.prefs, 
                strong.intensities.of.prefs = strong.intensities.of.prefs, 
                weak.intensities.of.prefs = weak.intensities.of.prefs, 
                indif.intensities.of.prefs = indif.intensities.of.prefs, 
                rank.related.requirements = rank.related.requirements, 
                nums.of.characteristic.points = nums.of.characteristic.points, 
                num.of.samples = num.of.samples, criteria.by.nodes = criteria.by.nodes, 
                nodeid = node.id)
            results[[node.id]] = result
        }
    }
    return(results)
  }
}

