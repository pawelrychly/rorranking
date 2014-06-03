
findRepresentativeValueFunction <- function(perf, strict.vf=FALSE, 
                                             strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                                             strong.intensities.of.prefs = NULL,
                                             weak.intensities.of.prefs = NULL,
                                             indif.intensities.of.prefs = NULL, 
                                             rank.related.requirements = NULL, 
                                             nums.of.characteristic.points = NULL,
                                             nec.relations.matrix=NULL, is.compromise=FALSE, k=0.0) {
  func <- list()
  if (is.compromise) {
    func <- findRepresentativeValueFunctionCompromise(perf, strict.vf=strict.vf, 
                                                     strong.prefs = strong.prefs, weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                                     strong.intensities.of.prefs = strong.intensities.of.prefs,
                                                     weak.intensities.of.prefs = weak.intensities.of.prefs,
                                                     indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                     rank.related.requirements = rank.related.requirements, 
                                                     nums.of.characteristic.points = nums.of.characteristic.points,
                                                     nec.relations.matrix=nec.relations.matrix) 
  } else {
    func <- findRepresentativeValueFunctionIterative(perf, strict.vf=strict.vf, 
                                                      strong.prefs = strong.prefs, weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                                      strong.intensities.of.prefs = strong.intensities.of.prefs,
                                                      weak.intensities.of.prefs = weak.intensities.of.prefs,
                                                      indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                      rank.related.requirements = rank.related.requirements, 
                                                      nums.of.characteristic.points = nums.of.characteristic.points,
                                                      nec.relations.matrix=nec.relations.matrix, k=k) 
  }
  return(func)
}

findRepresentativeValueFunctionHierarchical <-  function(perf, strict.vf=FALSE, 
                                                         strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                                                         strong.intensities.of.prefs = NULL,
                                                         weak.intensities.of.prefs = NULL,
                                                         indif.intensities.of.prefs = NULL, 
                                                         rank.related.requirements = NULL, 
                                                         nums.of.characteristic.points = NULL,
                                                         nec.relations=NULL, is.compromise=FALSE, k=0.0, hierarchy.data=NULL) {
  
  results <- list()
  hierarchy.data <- prepareHierarchyData(perf=perf, hierarchy.data=hierarchy.data)
  err <- NULL
  if (hierarchy.data[["status"]] == "OK") {
    nodes <- hierarchy.data$nodes
    criteria.by.nodes <- hierarchy.data$criteria.by.nodes 
    
    for (node.id in nodes) {
      func <- list()
      if (is.compromise) {
        func <- findRepresentativeValueFunctionCompromise(perf, strict.vf=strict.vf, 
                                                 strong.prefs = strong.prefs, weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                                 strong.intensities.of.prefs = strong.intensities.of.prefs,
                                                 weak.intensities.of.prefs = weak.intensities.of.prefs,
                                                 indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                 rank.related.requirements = rank.related.requirements, 
                                                 nums.of.characteristic.points = nums.of.characteristic.points,
                                                 nec.relations.matrix=nec.relations[[node.id]],
                                                 criteria.by.nodes=criteria.by.nodes, nodeid=node.id) 
      } else {
        func <- findRepresentativeValueFunctionIterative(perf, strict.vf=strict.vf, 
                                                         strong.prefs = strong.prefs, weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                                         strong.intensities.of.prefs = strong.intensities.of.prefs,
                                                         weak.intensities.of.prefs = weak.intensities.of.prefs,
                                                         indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                         rank.related.requirements = rank.related.requirements, 
                                                         nums.of.characteristic.points = nums.of.characteristic.points,
                                                         nec.relations.matrix=nec.relations[[node.id]],
                                                         k=k,  criteria.by.nodes=criteria.by.nodes, nodeid=node.id) 
      }
      results[[node.id]] = func
    }  
  } 
  
  return(results)
}