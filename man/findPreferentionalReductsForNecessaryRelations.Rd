\name{findPreferentionalReductsForNecessaryRelations}
\alias{findPreferentionalReductsForNecessaryRelations}
\title{Finds all rank related preferentional reducts.}
\description{Function finds all preferentional reducts for a each necessary relation}
\usage{
findPreferentionalReductsForNecessaryRelations(perf, strict.vf, strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL, nums.of.characteristic.points = NULL, criteria=NULL, nec.relations.matrix = NULL)
}
\arguments{
  \item{perf}{
  m x n performance matrix with m alternatives and n criteria}
  \item{strict.vf}{
  Whether to use sctrictly increasing (TRUE) or monotonously increasing (FALSE) value functions.}
  \item{strong.prefs}{
  k x 2 matrix of strong preferences statements (row indices of alternatives in the performance matrix). Each row r is a preference statements meaning that preferences[k,1] is strongly preferred to preferences[k,2].}
  \item{weak.prefs}{
  k x 2 matrix of strong preferences statements (row indices of alternatives in the performance matrix). Each row r is a preference statements meaning that preferences[k,1] is strongly preferred to preferences[k,2]}
  \item{indif.prefs}{
  k x 2 matrix of indifference preferences statements (row indices of alternatives in the performance matrix). Each row r is a preference statements meaning that preferences[k,1] is equally preferred to preferences[k,2]. }
  \item{nums.of.characteristic.points}{ List of integers. value on n-th position, describes number of characteristic points used in n-th criterion. If value is 0, then no characteristic points are used. }
  \item{criteria}{ List of characters. value on n-th position, determines the type of n-th criterion. If value equals "c" then criterion is cost. Otherwise the type of criterion is gain. }
  \item{nec.relations.matrix}{
    m x 2  matrix with m alternatives. First column describes the worst possible ranking. The second column describes the best possible ranking position of variant.
  }
}
\value{
A list keyed by an necessary relations. Each value, is a list of reducts. Reduct is described as an list of strings which describes a preference relations given by DM.
}

\author{
Pawel Rychly
}

\seealso{
  \code{\link{findPreferentionalReductsForNecessaryRelationsHierarchical}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (perf, strict.vf, strong.prefs = NULL, weak.prefs = NULL, 
    indif.prefs = NULL, nums.of.characteristic.points = NULL, 
    nec.relations.matrix = NULL) 
{
    reducts <- findReductsForNecRelations(perf = perf, strict.vf = strict.vf, 
        strong.prefs = strong.prefs, weak.prefs = weak.prefs, 
        indif.prefs = indif.prefs, nums.of.characteristic.points = nums.of.characteristic.points, 
        nec.relations.matrix = nec.relations.matrix)
    return(reducts)
  }
}

