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

getPerformancesFromXmcdaFiles <- function(alternatives.filename, criteria.filename, performances.filename) {
  
  treeAlternatives <- NULL
  treeCriteria <- NULL
  treePerformanceTable <- NULL
  
  tmpErr <- try( {
    treeAlternatives<-xmlTreeParse(alternatives.filename,useInternalNodes=TRUE)
  }, silent=TRUE)
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read alternatives file.", 
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(treeAlternatives) == 0) {
    return(list(status="ERROR", errFile ="Alternatives file is not XMCDA valid.",
                errData=NULL, data=NULL))  
  }
  
  tmpErr <- try({
    treeCriteria<-xmlTreeParse(criteria.filename,useInternalNodes=TRUE)
  }, silent=TRUE)
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read criteria file.",
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(treeCriteria) == 0) {
    return(list(status="ERROR", errFile="Criteria file is not XMCDA valid.",
                errData=NULL, data=NULL))
  }
  
  tmpErr <- try({
    treePerformanceTable<-xmlTreeParse(performances.filename,useInternalNodes=TRUE)
  }, silent=TRUE)
  
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


getCharacteristicPointsFromXmcdaFile <- function(filename, performances){
  treePoints <- NULL
  tmpErr<-try(
    {
      treePoints<-xmlTreeParse(filename,useInternalNodes=TRUE)
    }, silent=TRUE
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
    for (row_num in 1:nrow(values[[1]])) {
      row <- values[[1]][row_num,]
      characteristicPoints[row[1]] <- row[2] 
    } 
    return(list(status="OK", errFile=NULL,
                errData=NULL, data=characteristicPoints))
  } else {
    return(list(status="ERROR", errFile=NULL,
                errData=values$status, data=NULL))  
  }
}

getParametersDataFromXmcdaFile <- function(filename, keys, defaults=list()) {
  tree <- NULL
  tmpErr<- try(
    {
      tree<-xmlTreeParse(filename,useInternalNodes=TRUE)  
    }, silent=TRUE
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


getAlternativesComparisonsByTypeOfPreference <- function(tree, altIDs=NULL, preferenceTypes = c("strong", "weak", "indif")){
  
  # if an mcdaConcept has been specified, search according to this attribute
  alternativesComparisons <- getNodeSet(tree, paste("//alternativesComparisons",sep=""))
  out<-list(preferences=list(), intensities.of.preferences=list())
  err1<-NULL
  err2<-NULL
  
  for (key in preferenceTypes) {
    out$preferences[[key]] = NULL
    out$intensities.of.preferences[[key]] = NULL
  }

  if (length(alternativesComparisons)>0){
    for (i in 1:length(alternativesComparisons)){
      alternativesComp <- matrix(nrow=0,ncol=3)
      alternativesPairsComp <- matrix(nrow=0,ncol=5)

      type <- xmlValue(getNodeSet(alternativesComparisons[[i]], "comparisonType")[[1]])
      pairs <- getNodeSet(alternativesComparisons[[i]], "pairs/pair")
      if (!(type %in% preferenceTypes)) {
        return(list(status="ERROR", errFile =NULL,
                    errData="Incorrect label of preference relation", data=NULL)) 
      }
      if (length(pairs)>0){
        for (j in 1:length(pairs)){
          
          alternatives <- list()
          val<-NULL
          noPairs<-FALSE
          tmpErr<-try(
            {
              alternatives<- append(alternatives, getNodeSet(pairs[[j]], "initial/alternativeID"))
              alternatives<- append(alternatives, getNodeSet(pairs[[j]], "terminal/alternativeID"))
            }, silent=TRUE
          )
          if (inherits(tmpErr, 'try-error') || (length(alternatives) == 0)) {
            alternatives <- list()
            tmpErr1<-try(
              {
                initials <- getNodeSet(pairs[[j]], "initial/alternativesSet/element/alternativeID")
                terminals <- getNodeSet(pairs[[j]], "terminal/alternativesSet/element/alternativeID") 
                alternatives <- list(initials[[1]], initials[[2]], terminals[[1]], terminals[[2]])  
              }, silent=TRUE
            ) 
            if (inherits(tmpErr1, 'try-error') && inherits(tmpErr, 'try-error')) {
              return(list(status="ERROR", errFile =NULL,
                          errData="Impossible to read (a) value(s) in a <alternativesComparisons>.", data=NULL)) 
              
            }
          }
          
          tmpErr2<-try(
            {
              val <- xmlValue(getNodeSet(pairs[[j]], "value/label")[[1]])
            }, silent=TRUE
          )
          if (inherits(tmpErr2, 'try-error')) {
            val <- 0 
          }
          
          if (length(alternatives) == 2) {
            if (((xmlValue(alternatives[[1]])%in%altIDs)&(xmlValue(alternatives[[2]])%in%altIDs))|(is.null(altIDs))) {
              alternativesComp <-  rbind(alternativesComp,c(xmlValue(alternatives[[1]]),xmlValue(alternatives[[2]]),val))
            } 
          } else if (length(alternatives) == 4) {
            if (((xmlValue(alternatives[[1]])%in%altIDs) & 
                   (xmlValue(alternatives[[2]])%in%altIDs) & 
                   (xmlValue(alternatives[[3]])%in%altIDs) & 
                   (xmlValue(alternatives[[4]])%in%altIDs)) | (is.null(altIDs))) {
               alternativesPairsComp <-  rbind(alternativesPairsComp,c(xmlValue(alternatives[[1]]),xmlValue(alternatives[[2]]), xmlValue(alternatives[[3]]),xmlValue(alternatives[[4]]),val))
            }
          }
        } 
      } 

      if (dim(alternativesComp)[1] == 0) {
        alternativesComp <- NULL
      }
      if (dim(alternativesPairsComp)[1] == 0) {
        alternativesPairsComp <- NULL
      }
      if (is.matrix(out$preferences[[type]])) {
        out$preferences[[type]] <- rbind(out$preferences[[type]], alternativesComp) 
      } else {
        out$preferences[[type]] <- alternativesComp
      }
      if (is.matrix(out$intensities.of.preferences[[type]])) {
        out$intensities.of.preferences[[type]] <- rbind(out$intensities.of.preferences[[type]], alternativesPairsComp) 
      } else {
        out$intensities.of.preferences[[type]] <- alternativesPairsComp   
      }
      
    } 
  } 
  return(list(status="OK", errFile =NULL,
              errData=NULL, data=out)) 
  
}



getPreferencesFromXmcdaFile <- function(filename, performances) {  
  treeAltComparison <- NULL
  tmpErr<-try(
    {
        treeAltComparison<-xmlTreeParse(filename,useInternalNodes=TRUE)
    }, silent=TRUE
  )
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read preferences.xml file.",
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(treeAltComparison) == 0) {
    return(list(status="ERROR", errFile="Preferences file is not XMCDA valid.",
                errData=NULL, data=NULL))
  }
  
  errData <- NULL
  flag <- TRUE
  cmp <- NULL
  
  cmpFrame <- getAlternativesComparisonsByTypeOfPreference(treeAltComparison, altIDs = rownames(performances))
  return(list(status=cmpFrame$status, errFile=cmpFrame$errFile, errData=cmpFrame$errData, data=cmpFrame$data$preferences))
}


getIntensitiesOfPreferencesFromXmcdaFile <- function(filename, performances) {  
  treeAltComparison <- NULL
  tmpErr<-try(
    {
      treeAltComparison<-xmlTreeParse(filename,useInternalNodes=TRUE)
    }, silent=TRUE
  )
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read intensities-of-preferences.xml file.",
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(treeAltComparison) == 0) {
    return(list(status="ERROR", errFile="Intensities of Preferences file is not XMCDA valid.",
                errData=NULL, data=NULL))
  }
  
  errData <- NULL
  flag <- TRUE
  cmp <- NULL
  
  cmpFrame <- getAlternativesComparisonsByTypeOfPreference(treeAltComparison, altIDs = rownames(performances))
  return(list(status=cmpFrame$status, errFile=cmpFrame$errFile, errData=cmpFrame$errData, data=cmpFrame$data$intensities.of.preferences))
}


getAlternativesIntervalValuesWithNodeData <- function(tree, altIDs){
  
  alternativesValues <- getNodeSet(tree, paste("//alternativesValues",sep=""))
  
  # create the empty output list and the errors
  out<-list()
  err1<-NULL
  err2<-NULL
  criteriaValMatrix <- NULL
  if (length(alternativesValues)>0){
    for (i in 1:length(alternativesValues)){
      
      # check whether we only have <alternativeID> under <alternativeValue>
      # and <interval> under <value> (and not a real)
      test1<-getNodeSet(alternativesValues[[i]], "alternativeValue")
      test1.names<-NULL
      test2<-getNodeSet(alternativesValues[[i]], "alternativeValue/value")
      test2.names<-NULL
      tmpErr<-try(
        {
          for (k in 1:length(test1))
            test1.names<-c(test1.names,names(xmlChildren(test1[[k]])))
          for (k in 1:length(test2))
            test2.names<-c(test2.names,names(xmlChildren(test2[[k]])))
        }
      )
      if (inherits(tmpErr, 'try-error')) {
        return(list(status="ERROR", errFile =NULL,
                    errData="Impossible to read (a) value(s) in a <alternativesValues>.", data=NULL)) 
        
      }
      criteriaVal <- matrix(nrow=0,ncol=4)
      if (!("alternativesSet" %in% test1.names)& ("interval" %in% test2.names)){
        
        
        vals <- getNodeSet(alternativesValues[[i]], "alternativeValue")
        
        if (length(vals)>0){
          for (j in 1:length(vals)){
            tmpErr<-try(
              {
                alternativeID <- getNodeSet(vals[[j]], "alternativeID")
                lb <- getNodeSet(vals[[j]], "value/interval/lowerBound")
                ub <- getNodeSet(vals[[j]], "value/interval/upperBound")
                nodesid <- 0
                nodesid.xml <- getNodeSet(vals[[j]], "value/label")
                if (length(nodesid.xml) > 0) {
                  nodesid <- xmlValue(nodesid.xml[[1]])
                } 
                if (length(which(altIDs==xmlValue(alternativeID[[1]])))>0) {
                  criteriaVal <-rbind(criteriaVal,c(xmlValue(alternativeID[[1]]),getNumericValue(lb),getNumericValue(ub), nodesid))
                }
              }
            )
            if (inherits(tmpErr, 'try-error')) {
              return(list(status="ERROR", errFile =NULL,
                          errData="Impossible to read (a) value(s) in a <alternativesValues>.", data=NULL)) 
              
            }
          }
        } #if (length(vals)>0){
        if (dim(criteriaVal)[1] == 0) {
          criteriaVal <- NULL
        }
        criteriaValMatrix <- rbind(criteriaValMatrix, criteriaVal)  
      }
    }
  } 
  return(list(status="OK", errFile =NULL,
              errData=NULL, data=criteriaValMatrix)) 
}

getRankRelatedPreferencesFromXmcdaFile <- function(filename, performances) {  
  tree <- NULL
  tmpErr<-try(
    {
      tree<-xmlTreeParse(filename,useInternalNodes=TRUE)
    }, silent=TRUE
  )
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read rank related preferences xml file.",
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(tree) == 0) {
    return(list(status="ERROR", errFile="Rank related preference file is not XMCDA valid.",
                errData=NULL, data=NULL))
  }
  cmpFrame <- getAlternativesIntervalValuesWithNodeData(tree, altIDs=rownames(performances))
  return(list(status=cmpFrame$status, errFile=cmpFrame$errFile, errData=cmpFrame$errData, data=cmpFrame$data))
}