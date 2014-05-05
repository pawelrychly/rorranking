#getCriteriaByNodesMatrix <- function(perf, criteria.by.nodes) {
#  if (hierarchy.data[["status"]] != "OK") {
#    return(NULL)
#  }
#  mat <- matrix(data=FALSE, ncol=ncol(perf), nrow=length(criteria.by.nodes), dimnames=list(names(criteria.by.nodes), colnames(perf)))
#  for (nodeID in names(criteria.by.nodes)) {
#    for (criterionID in criteria.by.nodes[[nodeID]]) {
#      mat[nodeID, criterionID] = TRUE
#    }
#  }
#  return(mat)
#}

getListOfNodesFromHierarchy <- function(hierarchy, result.list=list()) {
  for (nodeID in names(hierarchy)) {
    if (length(hierarchy[[nodeID]]) > 0) {
      result.list <- getListOfNodesFromHierarchy(hierarchy[[nodeID]], result.list) 
    }
    result.list <- append(result.list, nodeID)
  }
  return(result.list)
}

prepareHierarchyData <- function(perf, hierarchy.data = NULL) {
  err <- NULL
  status <- "OK"
  results <- list()
  nodes <- list()
  criteria.by.nodes = NULL
  if ("hierarchy.tree" %in% names(hierarchy.data)) {
    nodes <- getListOfNodesFromHierarchy(hierarchy.data[["hierarchy.tree"]])
    if ((length(nodes) > 0) && ("criteria.by.nodes" %in% names(hierarchy.data))){
      criteria.by.nodes <- hierarchy.data[["criteria.by.nodes"]]  
    } else {
      status <- "Hierarchy data is incorrect."
    }
  } else {
    nodes <- list("results")
    criteria.by.nodes <- list("results" = colnames(perf))
  }
  return(list("nodes"=nodes, "criteria.by.nodes"=criteria.by.nodes, "status"=status))
}

