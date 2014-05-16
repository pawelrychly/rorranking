findMissingOrRedundantUtilityValueForRankPosition <- function(perf, a, k, strict.vf, is.possibly.preffered,
                                                                       strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                       strong.intensities.of.prefs = NULL,
                                                                       weak.intensities.of.prefs = NULL,
                                                                       indif.intensities.of.prefs = NULL, 
                                                                       rank.related.requirements = NULL,
                                                                       nums.of.characteristic.points=NULL,
                                                                       improvement=TRUE) {
  result <- pfaUmissingNecessaryOrPossibleComprehensiveRankImprovement(perf=perf, a=a, k=k, strict.vf=strict.vf, 
                                                                       is.possibly.preffered=is.possibly.preffered,
                                                                       strong.prefs=strong.prefs,
                                                                       weak.prefs=weak.prefs,
                                                                       indif.prefs = indif.prefs,
                                                                       strong.intensities.of.prefs = strong.intensities.of.prefs,
                                                                       weak.intensities.of.prefs = weak.intensities.of.prefs,
                                                                       indif.intensities.of.prefs = indif.intensities.of.prefs,
                                                                       rank.related.requirements = rank.related.requirements,
                                                                       nums.of.characteristic.points=nums.of.characteristic.points,
                                                                       improvement=improvement)
  return(result)
}

findNecessaryRankRelatedPerformanceModification <- function(perf, a, k, strict.vf, strong.prefs=NULL,
                                                           weak.prefs=NULL, indif.prefs = NULL, 
                                                           strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                           rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                           which.attributes = NULL, greater.than.one = TRUE) {
  result <- necessaryComprehensiveRankImprovement(perf=perf, a=a, k=k, strict.vf=strict.vf,
                                                 strong.prefs=strong.prefs, weak.prefs=weak.prefs, indif.prefs = indif.prefs, 
                                                 strong.intensities.of.prefs = strong.intensities.of.prefs,
                                                 weak.intensities.of.prefs = weak.intensities.of.prefs,
                                                 indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                 rank.related.requirements = rank.related.requirements,
                                                 nums.of.characteristic.points=nums.of.characteristic.points, precision=precision,
                                                 which.attributes = which.attributes, greater.than.one = greater.than.one)
  return(result)
}

findPossibleRankRelatedPerformanceModification <- function(perf, a, k, strict.vf, strong.prefs=NULL,
                                                           weak.prefs=NULL, indif.prefs = NULL, 
                                                           strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                           rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                           which.attributes = NULL, greater.than.one = TRUE) {
  result <- possibleComprehensiveRankImprovement(perf=perf, a=a, k=k, strict.vf=strict.vf,
                                                 strong.prefs=strong.prefs, weak.prefs=weak.prefs, indif.prefs = indif.prefs, 
                                                 strong.intensities.of.prefs = strong.intensities.of.prefs,
                                                 weak.intensities.of.prefs = weak.intensities.of.prefs,
                                                 indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                 rank.related.requirements = rank.related.requirements,
                                                 nums.of.characteristic.points=nums.of.characteristic.points, precision=precision,
                                                 which.attributes = which.attributes, greater.than.one = greater.than.one)
  return(result)
}

findMissingOrRedundantUtilityValueForRankPositionHierarchical <- function(perf, a, k, strict.vf, is.possibly.preffered,
                                                              strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                              strong.intensities.of.prefs = NULL,
                                                              weak.intensities.of.prefs = NULL,
                                                              indif.intensities.of.prefs = NULL, 
                                                              rank.related.requirements = NULL,
                                                              nums.of.characteristic.points = NULL,
                                                              improvement=TRUE, hierarchy.data = NULL) {
  results <- list()
  hierarchy.data <- prepareHierarchyData(perf, hierarchy.data)
  if (hierarchy.data[["status"]] == "OK") {
    nodes <- hierarchy.data$nodes
    criteria.by.nodes <- hierarchy.data$criteria.by.nodes 
    for (node.id in nodes) { 
      result <- pfaUmissingNecessaryOrPossibleComprehensiveRankImprovement(perf=perf, a=a, k=k, strict.vf=strict.vf, 
                                                                           is.possibly.preffered=is.possibly.preffered,
                                                                           strong.prefs=strong.prefs,
                                                                           weak.prefs=weak.prefs,
                                                                           indif.prefs = indif.prefs,
                                                                           strong.intensities.of.prefs = strong.intensities.of.prefs,
                                                                           weak.intensities.of.prefs = weak.intensities.of.prefs,
                                                                           indif.intensities.of.prefs = indif.intensities.of.prefs,
                                                                           rank.related.requirements = rank.related.requirements,
                                                                           nums.of.characteristic.points=nums.of.characteristic.points,
                                                                           improvement=improvement, criteria.by.nodes=criteria.by.nodes, nodeid=node.id)
      results[[node.id]] = result
    }  
  } 
  return(results)
}

findNecessaryRankRelatedPerformanceModificationHierarchical <- function(perf, a, k, strict.vf, strong.prefs=NULL,
                                                                       weak.prefs=NULL, indif.prefs = NULL, 
                                                                       strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                       rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                                       which.attributes = NULL, greater.than.one = TRUE, hierarchy.data=NULL) {
  
  results <- list()
  hierarchy.data <- prepareHierarchyData(perf, hierarchy.data)
  if (hierarchy.data[["status"]] == "OK") {
    nodes <- hierarchy.data$nodes
    criteria.by.nodes <- hierarchy.data$criteria.by.nodes 
    for (node.id in nodes) { 
      result <- necessaryComprehensiveRankImprovement(perf=perf, a=a, k=k, strict.vf=strict.vf,
                                                     strong.prefs=strong.prefs, weak.prefs=weak.prefs, indif.prefs = indif.prefs, 
                                                     strong.intensities.of.prefs = strong.intensities.of.prefs,
                                                     weak.intensities.of.prefs = weak.intensities.of.prefs,
                                                     indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                     rank.related.requirements = rank.related.requirements,
                                                     nums.of.characteristic.points=nums.of.characteristic.points, precision=precision,
                                                     which.attributes = which.attributes, greater.than.one = greater.than.one, criteria.by.nodes=criteria.by.nodes, nodeid=node.id)
      results[[node.id]] = result
    }  
  } 
  return(results)
}

findPossibleRankRelatedPerformanceModificationHierarchical <- function(perf, a, k, strict.vf, strong.prefs=NULL,
                                                           weak.prefs=NULL, indif.prefs = NULL, 
                                                           strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                           rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                           which.attributes = NULL, greater.than.one = TRUE, hierarchy.data=NULL) {
  
  results <- list()
  hierarchy.data <- prepareHierarchyData(perf, hierarchy.data)
  if (hierarchy.data[["status"]] == "OK") {
    nodes <- hierarchy.data$nodes
    criteria.by.nodes <- hierarchy.data$criteria.by.nodes 
    for (node.id in nodes) { 
      result <- possibleComprehensiveRankImprovement(perf=perf, a=a, k=k, strict.vf=strict.vf,
                                                     strong.prefs=strong.prefs, weak.prefs=weak.prefs, indif.prefs = indif.prefs, 
                                                     strong.intensities.of.prefs = strong.intensities.of.prefs,
                                                     weak.intensities.of.prefs = weak.intensities.of.prefs,
                                                     indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                     rank.related.requirements = rank.related.requirements,
                                                     nums.of.characteristic.points=nums.of.characteristic.points, precision=precision,
                                                     which.attributes = which.attributes, greater.than.one = greater.than.one, criteria.by.nodes=criteria.by.nodes, nodeid=node.id)
      results[[node.id]] = result
    }  
  } 
  return(results)
}




findPerformanceModificationForPossibleRelation <- function(perf, a, b, strict.vf, strong.prefs=NULL,
                                                            weak.prefs=NULL, indif.prefs = NULL, 
                                                            strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                            rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                            which.attributes = NULL, greater.than.one = TRUE) {
  result <-possibleComprehensiveImprovement(perf=perf, a=a, b=b, strict.vf=strict.vf, strong.prefs=strong.prefs,
                                                        weak.prefs=weak.prefs, indif.prefs = indif.prefs, 
                                                        strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                        rank.related.requirements = rank.related.requirements,  nums.of.characteristic.points=nums.of.characteristic.points, precision=precision,
                                                        which.attributes = which.attributes, greater.than.one = greater.than.one) 
  return(result)
}

findPerformanceModificationForNecessaryRelation <- function(perf, a, b, strict.vf, strong.prefs=NULL,
                                                             weak.prefs=NULL, indif.prefs = NULL, 
                                                             strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                             rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                             which.attributes = NULL, greater.than.one = TRUE) {
  result <-necessaryComprehensiveImprovement(perf=perf, a=a, b=b, strict.vf=strict.vf, strong.prefs=strong.prefs,
                                            weak.prefs=weak.prefs, indif.prefs = indif.prefs, 
                                            strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                            rank.related.requirements = rank.related.requirements,  nums.of.characteristic.points=nums.of.characteristic.points, precision=precision,
                                            which.attributes = which.attributes, greater.than.one = greater.than.one) 
  return(result)
}

findMissingOrRedundantUtilityValueForRelation <- function(perf, a, b, strict.vf, is.possibly.preffered,
                                               strong.prefs=NULL, weak.prefs=NULL, indif.prefs = NULL, 
                                               strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                               rank.related.requirements = NULL, nums.of.characteristic.points=NULL, improvement=TRUE) {
  result <- missingUtilityNecessaryOrPossibleComprehensiveImprovement(perf=perf, a=a, b=b, strict.vf = strict.vf, is.possibly.preffered=is.possibly.preffered,
                                                                        strong.prefs=strong.prefs, weak.prefs=weak.prefs, indif.prefs = indif.prefs, 
                                                                        strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                                        rank.related.requirements = rank.related.requirements, nums.of.characteristic.points=nums.of.characteristic.points, improvement=improvement) 
  return(result)
}

findPerformanceModificationForPossibleRelationHierarchical <- function(perf, a, b, strict.vf, strong.prefs=NULL,
                                                             weak.prefs=NULL, indif.prefs = NULL, 
                                                             strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                             rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                             which.attributes = NULL, greater.than.one = TRUE, hierarchy.data=hierarchy.data) {
  results <- list()
  hierarchy.data <- prepareHierarchyData(perf, hierarchy.data)
  if (hierarchy.data[["status"]] == "OK") {
    nodes <- hierarchy.data$nodes
    criteria.by.nodes <- hierarchy.data$criteria.by.nodes 
    for (node.id in nodes) {      
      result <-possibleComprehensiveImprovement(perf=perf, a=a, b=b, strict.vf=strict.vf, strong.prefs=strong.prefs,
                                                weak.prefs=weak.prefs, indif.prefs = indif.prefs, 
                                                strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                rank.related.requirements = rank.related.requirements,  nums.of.characteristic.points=nums.of.characteristic.points, precision=precision,
                                                which.attributes = which.attributes, greater.than.one = greater.than.one, criteria.by.nodes=criteria.by.nodes, nodeid=node.id) 
      results[[node.id]] = result
    }  
  } 
  return(results)
}

findPerformanceModificationForNecessaryRelationHierarchical <- function(perf, a, b, strict.vf, strong.prefs=NULL,
                                                                         weak.prefs=NULL, indif.prefs = NULL, 
                                                                         strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                         rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                                         which.attributes = NULL, greater.than.one = TRUE, hierarchy.data=hierarchy.data) {
  results <- list()
  hierarchy.data <- prepareHierarchyData(perf, hierarchy.data)
  if (hierarchy.data[["status"]] == "OK") {
    nodes <- hierarchy.data$nodes
    criteria.by.nodes <- hierarchy.data$criteria.by.nodes 
    for (node.id in nodes) {      
      result <- necessaryComprehensiveImprovement(perf=perf, a=a, b=b, strict.vf=strict.vf, strong.prefs=strong.prefs,
                                                weak.prefs=weak.prefs, indif.prefs = indif.prefs, 
                                                strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                rank.related.requirements = rank.related.requirements,  nums.of.characteristic.points=nums.of.characteristic.points, precision=precision,
                                                which.attributes = which.attributes, greater.than.one = greater.than.one, criteria.by.nodes=criteria.by.nodes, nodeid=node.id) 
      results[[node.id]] = result
    }  
  } 
  return(results)
}


findMissingOrRedundantUtilityValueForRelationHierarchical <- function(perf, a, b, strict.vf, is.possibly.preffered,
                                               strong.prefs=NULL, weak.prefs=NULL, indif.prefs = NULL, 
                                               strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                               rank.related.requirements = NULL, nums.of.characteristic.points=NULL, improvement=TRUE, hierarchy.data=NULL) {
  results <- list()
  hierarchy.data <- prepareHierarchyData(perf, hierarchy.data)
  if (hierarchy.data[["status"]] == "OK") {
    nodes <- hierarchy.data$nodes
    criteria.by.nodes <- hierarchy.data$criteria.by.nodes 
    for (node.id in nodes) {      
      result <- missingUtilityNecessaryOrPossibleComprehensiveImprovement(perf=perf, a=a, b=b, strict.vf = strict.vf, is.possibly.preffered=is.possibly.preffered,
                                                                          strong.prefs=strong.prefs, weak.prefs=weak.prefs, indif.prefs = indif.prefs, 
                                                                          strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                                          rank.related.requirements = rank.related.requirements, nums.of.characteristic.points=nums.of.characteristic.points, improvement=improvement, criteria.by.nodes=criteria.by.nodes, nodeid=node.id) 
      
      results[[node.id]] = result
    }  
  } 
  
  return(results)
}
