\name{findAllRankRelatedPreferentionalReducts}
\alias{findAllRankRelatedPreferentionalReducts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Finds all rank related preferentional reducts.
}
\description{
Function finds all preferentional reducts for a given best and worst possible ranking }
\usage{
findAllRankRelatedPreferentionalReducts(perf, ranks, strict.vf, strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL, nums.of.characteristic.points = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{perf}{
  m x n performance matrix with m alternatives and n criteria}
  \item{ranks}{
  m x 2  matrix with m alternatives. First column describes the worst possible ranking. The second column describes the best possible ranking position of variant.}
  \item{strict.vf}{
  Whether to use sctrictly increasing (TRUE) or monotonously increasing (FALSE) value functions.}
  \item{strong.prefs}{
  k x 2 matrix of strong preferences statements (row indices of alternatives in the performance matrix). Each row r is a preference statements meaning that preferences[k,1] is strongly preferred to preferences[k,2].}
  \item{weak.prefs}{
  k x 2 matrix of strong preferences statements (row indices of alternatives in the performance matrix). Each row r is a preference statements meaning that preferences[k,1] is strongly preferred to preferences[k,2]}
  \item{indif.prefs}{
  k x 2 matrix of indifference preferences statements (row indices of alternatives in the performance matrix). Each row r is a preference statements meaning that preferences[k,1] is equally preferred to preferences[k,2]. }
  \item{nums.of.characteristic.points}{ List of integers. value on n-th position, describes number of characteristic points used in n-th criterion. If value is 0, then no characteristic points are used. }
}

\value{ A list keyed by an alternative ids concatenated with information about their extreeme rankings. Each value, is a list of reducts. Reduct is described as an list of strings which describes a preference relations given by DM.}

\author{
Pawel Rychly
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (perf, ranks, strict.vf, strong.prefs = NULL, weak.prefs = NULL, 
    indif.prefs = NULL, nums.of.characteristic.points = NULL) 
{
    reducts <- findRankRelatedReducts(perf = perf, strict.vf = strict.vf, 
        ranks = ranks, strong.prefs = strong.prefs, weak.prefs = weak.prefs, 
        indif.prefs = indif.prefs, nums.of.characteristic.points = nums.of.characteristic.points)
    return(reducts)
  }
}

