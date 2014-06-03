findRankAcceptabilityIndices <- function(perf, 
                                         strict.vf, 
                                         strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                                         strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                         rank.related.requirements = NULL,
                                         nums.of.characteristic.points=NULL, num.of.samples=100) {
  
  
  ranks <- findRankAcceptabilityIndicesHelper(perf = perf, 
                                     strict.vf = strict.vf, 
                                     strong.prefs = strong.prefs, weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                     strong.intensities.of.prefs = strong.intensities.of.prefs,
                                     weak.intensities.of.prefs = weak.intensities.of.prefs,
                                     indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                     rank.related.requirements = rank.related.requirements,
                                     nums.of.characteristic.points=nums.of.characteristic.points,
                                     num.of.samples=num.of.samples)
  return(ranks)
}


findRankAcceptabilityIndicesHierarchical <- function(perf, 
                                         strict.vf, 
                                         strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                                         strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                         rank.related.requirements = NULL,
                                         nums.of.characteristic.points=NULL, num.of.samples=100, hierarchy.data=NULL) {
  results <- list()
  hierarchy.data <- prepareHierarchyData(perf, hierarchy.data)
  if (hierarchy.data[["status"]] == "OK") {
    nodes <- hierarchy.data$nodes
    criteria.by.nodes <- hierarchy.data$criteria.by.nodes 
    for (node.id in nodes) { 
      result <- findRankAcceptabilityIndicesHelper(perf = perf, 
                                                   strict.vf = strict.vf, 
                                                   strong.prefs = strong.prefs, weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                                   strong.intensities.of.prefs = strong.intensities.of.prefs,
                                                   weak.intensities.of.prefs = weak.intensities.of.prefs,
                                                   indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                   rank.related.requirements = rank.related.requirements,
                                                   nums.of.characteristic.points = nums.of.characteristic.points,
                                                   num.of.samples=num.of.samples, criteria.by.nodes=criteria.by.nodes, nodeid=node.id)
      results[[node.id]] = result
    }  
  } 
  return(results)
}


findRelationsAcceptabilityIndices <- function(perf, 
                                         strict.vf, 
                                         strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                                         strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                         rank.related.requirements = NULL,
                                         nums.of.characteristic.points=NULL, num.of.samples=100) {
  
  
  ranks <- findRelationsAcceptabilityIndicesHelper(perf = perf, 
                                              strict.vf = strict.vf, 
                                              strong.prefs = strong.prefs, weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                              strong.intensities.of.prefs = strong.intensities.of.prefs,
                                              weak.intensities.of.prefs = weak.intensities.of.prefs,
                                              indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                              rank.related.requirements = rank.related.requirements,
                                              nums.of.characteristic.points=nums.of.characteristic.points,
                                              num.of.samples=num.of.samples)
  return(ranks)
}


findRelationsAcceptabilityIndicesHierarchical <- function(perf, 
                                                     strict.vf, 
                                                     strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                                                     strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                     rank.related.requirements = NULL,
                                                     nums.of.characteristic.points=NULL, num.of.samples=100, hierarchy.data=NULL) {
  results <- list()
  hierarchy.data <- prepareHierarchyData(perf, hierarchy.data)
  if (hierarchy.data[["status"]] == "OK") {
    nodes <- hierarchy.data$nodes
    criteria.by.nodes <- hierarchy.data$criteria.by.nodes 
    for (node.id in nodes) { 
      result <- findRelationsAcceptabilityIndicesHelper(perf = perf, 
                                                   strict.vf = strict.vf, 
                                                   strong.prefs = strong.prefs, weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                                   strong.intensities.of.prefs = strong.intensities.of.prefs,
                                                   weak.intensities.of.prefs = weak.intensities.of.prefs,
                                                   indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                   rank.related.requirements = rank.related.requirements,
                                                   nums.of.characteristic.points = nums.of.characteristic.points,
                                                   num.of.samples=num.of.samples, criteria.by.nodes=criteria.by.nodes, nodeid=node.id)
      results[[node.id]] = result
    }  
  } 
  return(results)
}