#library("RXMCDA")

getHierarchyTree <- function(nodes){
  nodesXML <- xmlChildren(nodes)
  num.of.node.elements <- sum(names(nodesXML) == "node")
  hierarchy = list()
  if (num.of.node.elements > 0) {
    for (i in 1:num.of.node.elements){
      nodeXML <- nodesXML[[i]]
      id <- xmlGetAttr(nodeXML, "id")
      hierarchy[[id]]  <- getHierarchyTree(nodeXML)
    }
    return(hierarchy)
  } 
  return(list())
}

getCriteriaFromChildNodes <- function(nodes, criteria.by.nodes = list(), nodeid = NULL) {
  nodesXML <- xmlChildren(nodes)
  num.of.node.elements <- sum(names(nodesXML) == "node")
  if (num.of.node.elements > 0) {
    child.ids <- list()
    for (i in 1:num.of.node.elements){
      nodeXML <- nodesXML[[i]]
      id <- xmlGetAttr(nodeXML, "id")
      criteria.by.nodes <- getCriteriaFromChildNodes(nodeXML, criteria.by.nodes, nodeid = id)
      child.ids <- append(child.ids, id)
    }
    if (!is.null(nodeid)) {
      for (id in child.ids) {
        criteria.by.nodes[[nodeid]] <- unique(append(criteria.by.nodes[[nodeid]], criteria.by.nodes[[id]]))      
      }  
    }
    return(criteria.by.nodes)
  } else {
    if ("criteriaSet" %in% names(nodesXML)) {
      
      criterionIDs <- sapply(xmlChildren(nodesXML[[1]]),function(element){
        criterionID <- xmlChildren(xmlChildren(element)[[1]])$text
        return(criterionID)
      })  
      names(criterionIDs) <- c()
      if (!is.null(nodeid)) {
        criteria.by.nodes[[nodeid]] <- unique(append(criteria.by.nodes[[nodeid]], criterionIDs))
      }
    }
    return(criteria.by.nodes)
  }
}

getHierarchyOfCriteriaTree <- function(tree){
  
  err<-NULL
  hierarchy.tree <- list()
  criteria.by.nodes <- list()
  hierarchySrc <- getNodeSet(tree, "//hierarchy[1]");
  if (length(hierarchySrc) > 0) {
    nodes <- hierarchySrc[[1]]
    criteria.by.nodes <- getCriteriaFromChildNodes(nodes) 
    hierarchy.tree <- getHierarchyTree(nodes)
  }
  status <- ""
  if ((length(hierarchy.tree) == 0) || (length(criteria.by.nodes) == 0)) {
    status <- paste(status, "Hierarchy data is incorrect.")
  } else {
    status <- "OK"
  }
  
  return(list("criteria.by.nodes"=criteria.by.nodes, "hierarchy.tree"=hierarchy.tree, "status"=status))
}




#errFile <-NULL
#tmpErr <- try(
#  treeCriteria<-xmlTreeParse("in1/hierarchy-of-criteria.xml",useInternalNodes=TRUE)
#)
#if (inherits(tmpErr, 'try-error')) {
#  errFile <<- "Cannot read criteria file."
#}
#if (is.null(errFile)) {
#  if (checkXSD(treeCriteria) == 0) {
#    errFile <<- " Criteria file is not XMCDA valid."
#  }
#}

#if (is.null(errFile)) {
#  flag <- TRUE
#  hierarchy <- getHierarchyOfCriteriaTree(treeCriteria)
#  print(hierarchy)
#}


