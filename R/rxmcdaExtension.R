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




########################################
####### helpers ########################
########################################

getPerformancesFromXmcdaFiles <- function() {
  
  treeAlternatives <- NULL
  treeCriteria <- NULL
  treePerformanceTable <- NULL
  
  tmpErr <- try(
    treeAlternatives<-xmlTreeParse("alternatives.xml",useInternalNodes=TRUE)
  )
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read alternatives file.", 
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(treeAlternatives) == 0) {
    return(list(status="ERROR", errFile ="Alternatives file is not XMCDA valid.",
                errData=NULL, data=NULL))  
  }
  
  tmpErr <- try(
    treeCriteria<-xmlTreeParse("criteria.xml",useInternalNodes=TRUE)
  )
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read criteria file.",
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(treeCriteria) == 0) {
    return(list(status="ERROR", errFile="Criteria file is not XMCDA valid.",
                errData=NULL, data=NULL))
  }
  
  tmpErr <- try(
    treePerformanceTable<-xmlTreeParse("performances.xml",useInternalNodes=TRUE)
  )
  
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile= "Cannot read performance file.",
                errData=NULL, data=NULL))
  }
  
  if (checkXSD(treePerformanceTable) == 0) {
    return(list(status="ERROR", errFile ="Performances file is not XMCDA valid.",
                errData=NULL, data=NULL))
  }
  
  altIDsFrame <- NULL
  critIDsFrame <- NULL
  performancesFrame <- NULL
  critIDs <- NULL
  altIDs <- NULL
  performances <- NULL
 
  flag <- TRUE
  critIDsFrame <- getCriteriaIDs(treeCriteria)
  if (critIDsFrame$status == "OK") {
    critIDs <- critIDsFrame[[1]] 
    altIDsFrame <- getAlternativesIDs(treeAlternatives)
  } else {
    return(list(status="ERROR", errFile = NULL,
                errData = critIDsFrame$status, data=NULL)) 
  }
  if (altIDsFrame$status == "OK") {
    altIDs <- altIDsFrame[[1]]
    performancesFrame <- getPerformanceTables(treePerformanceTable,altIDs = altIDs, critIDs = critIDs)    
  } else {
    return(list(status="ERROR", errFile = NULL,
                errData = altIDsFrame$status, data=NULL)) 
  }
  
  if (performancesFrame$status == "OK") {
    performances <- performancesFrame[[1]]
  } else {
    return(list(status="ERROR", errFile = NULL,
                errData = performancesFrame$status, data=NULL))  
  }
  return(list(status="OK", errFile = NULL,
              errData = NULL, data=performances)) 
}


getCharacteristicPointsFromXmcdaFile <- function(performances){
  treePoints <- NULL
  tmpErr<-try(
    {
      treePoints<-xmlTreeParse("characteristic-points.xml",useInternalNodes=TRUE)
    }
  )  
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read characteristic-points.xml file.", 
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(treePoints)==0) {
    return(list(status="ERROR", errFile = "Characteristic Points File is not XMCDA valid.",
                errData=NULL, data=NULL))  
  }
  
  values <- getCriteriaValues(treePoints, colnames(performances))
  if (values$status == "OK") {
    characteristicPoints <- rep(0, length(colnames(performances)))
    for (row_num in 1:nrow(values$characteristicPoints)) {
      row <- values$characteristicPoints[row_num,]
      characteristicPoints[row[1]] <- row[2] 
    } 
    return(list(status="OK", errFile=NULL,
                errData=NULL, data=characteristicPoints))
  } else {
    return(list(status="ERROR", errFile=NULL,
                errData=values$status, data=NULL))  
  }
}

getParametersDataFromXmcdaFile <- function(keys, defaults=list()) {
  tree <- NULL
  tmpErr<- try(
    {
      tree<-xmlTreeParse("parameters.xml",useInternalNodes=TRUE)  
    }
  )
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read parameters.xml file.",
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(tree) == 0) {
    return(list(status="ERROR", errFile="File with parameters values is not XMCDA valid.",
                errData=NULL, data=NULL))
  }
  parameters <- list()
  params <- getParameters(tree)
  for (key in keys) {
    if (key %in% names(params)) {
      parameters[[key]] <- params[[key]]
    } else if (key %in% names(defaults)) {
      parameters[[key]] <- defaults[[key]]
    } 
  }
  return(list(status="OK", errFile=NULL,
                errData=NULL, data=parameters))
}
