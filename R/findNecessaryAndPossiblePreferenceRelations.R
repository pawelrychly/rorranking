  
findPreferenceRelations <- function(perf, 
                                   strict.vf, 
                                   strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                                   strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                   rank.related.requirements = NULL,
                                   nums.of.characteristic.points=NULL, criteria=NULL, criteria.by.nodes=NULL, nodeid=NULL) {


  base.model <- buildBaseLPModel(perf, strict.vf, strong.prefs = strong.prefs,
                                 weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                 strong.intensities.of.prefs =  strong.intensities.of.prefs , weak.intensities.of.prefs = weak.intensities.of.prefs,
                                 indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                 rank.related.requirements = rank.related.requirements,
                                 nums.of.characteristic.points=nums.of.characteristic.points, criteria=criteria, criteria.by.nodes=criteria.by.nodes)
  
 
  number.of.real.variables <- getNumberOfVariables(perf=perf, numbers.of.characteristic.points=nums.of.characteristic.points)
  eps.position <- getEpsPosition(perf)
  if (!checkConstraintsConsistency(model=base.model, number.of.real.variables=number.of.real.variables, eps.position=eps.position)) {
    
    stop("Model infeasible")
  }
  
  variables.set <- NULL
  filter <- NULL
  if ((!is.null(nodeid)) && (!is.null(criteria.by.nodes))) {
    criteria.set <- criteria.by.nodes[[nodeid]]
    filter <- getCriteriaFilter(perf=perf, criteria=criteria.set)
  }
  
  nec.rel <- findNecessaryOrPossibleRelations(perf, base.model=base.model, number.of.real.variables = number.of.real.variables, necessary=TRUE, filter=filter)
  pos.rel <- findNecessaryOrPossibleRelations(perf, base.model=base.model, number.of.real.variables = number.of.real.variables, necessary=FALSE, filter=filter)

  result <- list(nec.relations=nec.rel, pos.relations = pos.rel)
  return(result)
}


findNecessaryAndPossiblePreferenceRelations <- function(perf, 
                                                        strict.vf, 
                                                        strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                                                        strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                        rank.related.requirements = NULL,
                                                        nums.of.characteristic.points=NULL, criteria=NULL) {
  relations <- findPreferenceRelations(perf=perf, strict.vf=strict.vf, strong.prefs = strong.prefs,
                                       weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                       strong.intensities.of.prefs =  strong.intensities.of.prefs , weak.intensities.of.prefs = weak.intensities.of.prefs,
                                       indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                       rank.related.requirements = rank.related.requirements,
                                       nums.of.characteristic.points=nums.of.characteristic.points, criteria=criteria)
  return(relations)
}

findNecessaryAndPossiblePreferenceRelationsHierarchical <- function(perf, 
                                                        strict.vf, 
                                                        strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                                                        strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                        rank.related.requirements = NULL,
                                                        nums.of.characteristic.points=NULL, criteria=NULL, hierarchy.data=NULL) {
  

  results <- list()
  hierarchy.data <- prepareHierarchyData(perf, hierarchy.data)
  err <- NULL
  if (hierarchy.data[["status"]] == "OK") {
    nodes <- hierarchy.data$nodes
    criteria.by.nodes <- hierarchy.data$criteria.by.nodes 
    
    for (node.id in nodes) {
      relations <- findPreferenceRelations(perf=perf, strict.vf=strict.vf, strong.prefs = strong.prefs,
                                weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                strong.intensities.of.prefs =  strong.intensities.of.prefs , weak.intensities.of.prefs = weak.intensities.of.prefs,
                                indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                rank.related.requirements = rank.related.requirements,
                                nums.of.characteristic.points=nums.of.characteristic.points, criteria=criteria, criteria.by.nodes=criteria.by.nodes, nodeid=node.id)
      
      results[['nec.relations']][[node.id]] <- relations[['nec.relations']]
      results[['pos.relations']][[node.id]] <- relations[['pos.relations']]
      
      #results[[node.id]] = relations
    }  
  }
  
  return(results)
  #return(list(nec.relations=necessary.relations, pos.relations=possible.relations))
}