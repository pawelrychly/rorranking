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
        criterionID <- xmlValue(xmlChildren(xmlChildren(element)[[1]])$text)
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

getHierarchyOfCriteriaFromXmcdaFile <- function(filename) {  
  tree <- NULL
  tmpErr<-try(
    {
       tree<-xmlTreeParse(filename,useInternalNodes=TRUE)
    }, silent=TRUE
  )
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read hierarchy-of-criteria.xml file.",
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(tree) == 0) {
    return(list(status="ERROR", errFile="Hierarchy file is not XMCDA valid.",
                errData=NULL, data=NULL))
  }
  
  errData <- NULL
  flag <- TRUE
  cmp <- NULL
  hierarchy = getHierarchyOfCriteriaTree(tree)
  return(list(status="OK", errFile=NULL, errData=NULL, data=hierarchy))
}

########################################
####### helpers ########################
########################################

getFirstAlternativeValue <- function(tree){
  alternativeValue <- getNodeSet(tree, paste("//alternativeValue",sep=""))
  altValue <- NULL  
  result <- list()
  if (length(alternativeValue)>0){
    tmpErr<-try(
      {
        alternativeID <- getNodeSet(alternativeValue[[1]], "alternativeID")
        value <- getNodeSet(alternativeValue[[1]], "value")
        value <- getNumericValue(value)
        result[[xmlValue(alternativeID[[1]])]] <- value
      }
    )
    if (inherits(tmpErr, 'try-error')){
      return(list(status="ERROR", errFile="Impossible to read a value in a <alternativeValue>.",
                errData=NULL, data=NULL))
    }
  } else {#if (length(alternativesValues)>0){
    return(list(status="ERROR", errFile="No <alternativeValue> found.",
                errData=NULL, data=NULL))
  }
  return(list(status="OK", data=result, errFile=NULL, errData=NULL))
}

getCriteriaSet <- function(tree){
  
  criteria <- getNodeSet(tree, paste("//criteriaSet",sep=""))
  criteriaIDs <- c()
  if (length(criteria)>0){
    for (i in 1:length(criteria)){
      elements <- getNodeSet(criteria[[i]], "element")
      if (length(elements)>0){
        for (j in 1:length(elements)){
          criterionID<-getNodeSet(elements[[j]], "criterionID")
          criteriaIDs<-c(criteriaIDs,xmlValue(criterionID[[1]]))
        }
      }
    }
  }
  else { #if (length(criteria)>0){
    return(list(status="ERROR", errFile="No <criteriaSet> found.",
                errData=NULL, data=NULL))
  }
  return(list(status="OK", data=criteriaIDs, errFile=NULL, errData=NULL))
}

getSelectedCriteriaFromXmcdaFile <- function(filename, criteriaIDs){
  tree <- NULL
  filter <- rep(0, length(criteriaIDs))
  tmpErr <- try(
    {
      tree <- xmlTreeParse(filename, useInternalNodes=TRUE)
    }, silent=TRUE
  )
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read selected criteria file.", 
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(tree) == 0) {
    return(list(status="ERROR", errFile ="Selected criteria file is not XMCDA valid.",
                errData=NULL, data=NULL))  
  }
  value <- getCriteriaSet(tree)
  if (value$status=="OK") {
    for (criterionID in value$data) {
       filter[[which(criteriaIDs == criterionID)]] <- 1
    }
    return(list(status="OK", errFile = NULL,
              errData = NULL, data=filter)) 
  } else {
    return(list(status="ERROR", errFile = value$errFile,
                errData = value$errData, data=NULL)) 
  }
}


getFirstAlternativeValueFromXmcdaFile <- function(alternative.value.filename) {
  tree <- NULL
  tmpErr <- try(
    {
      tree <- xmlTreeParse(alternative.value.filename, useInternalNodes=TRUE)
    }, silent=TRUE
  )
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read alternative value file.", 
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(tree) == 0) {
    return(list(status="ERROR", errFile ="Alternative value file is not XMCDA valid.",
                errData=NULL, data=NULL))  
  }
  value <- getFirstAlternativeValue(tree)
  if (value$status=="OK") {
     return(list(status="OK", errFile = NULL,
              errData = NULL, data=value$data)) 
  } else {
    return(list(status="ERROR", errFile = value$errFile,
                errData = value$errData, data=NULL)) 
  }
}

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
      comparisonType <- getNodeSet(alternativesComparisons[[i]], "comparisonType")
      if (length(comparisonType) < 1) {
        return(list(status="ERROR", errFile =NULL,
                          errData="ComparisonType in  <alternativesComparisons> is not given.", data=NULL)) 
              
      }

      type <- xmlValue(comparisonType[[1]])
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

getNecessaryRelationsMatrixFromXmcdaFile <- function(filename, performances){
  treePoints <- NULL
  tmpErr<-try(
    {
      tree<-xmlTreeParse(filename,useInternalNodes=TRUE)
    }, silent=TRUE
  )  
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read necessary-relations.xml file.", 
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(tree)==0) {
    return(list(status="ERROR", errFile = "Necessary relations File is not XMCDA valid.",
                errData=NULL, data=NULL))  
  }
  relations <- getAlternativesComparisonsLabels(tree, rownames(performances))
  if (relations$status == "OK") {
    alt.ids = dimnames(performances)[[1]]
    
    nec.relations.matrix = matrix(data=FALSE, ncol=nrow(performances), nrow=nrow(performances),
                                  dimnames=list(alt.ids, alt.ids))
    for(i in seq(nrow(relations[[1]]))) {
        id1 = relations[[1]][i, 1]
        id2 = relations[[1]][i, 2]
        nec.relations.matrix[id1, id2] = TRUE 
    }
    return(list(status="OK", errFile=NULL,
                errData=NULL, data=nec.relations.matrix))
  } else {
    return(list(status="ERROR", errFile=NULL,
                errData=relations$status, data=NULL))  
  }
}


putAlternativesComparisonsWithReductsData <- function (tree, relations, reducts.by.relations, attributes=c()) 
{
  out <- list()
  err1 <- NULL
  err2 <- NULL
  root <- NULL
  tmpErr <- try({
    root <- xmlRoot(tree)
  })
  if (inherits(tmpErr, "try-error")) {
    err1 <- "No <xmcda:XMCDA> found."
  }
  if (length(root) != 0) {
    altVals <- newXMLNode("alternativesComparisons", attrs = attributes, parent = root, namespace = c())
    pairs <- newXMLNode("pairs", parent = altVals, namespace = c())
    relations.ids <- names(relations)
    for (id in relations.ids) {
      tmpErr <- try({
        pair <- newXMLNode("pair", parent = pairs, namespace = c())
        initial <- newXMLNode("initial", parent = pair, 
                              namespace = c())
        newXMLNode("alternativeID", relations[[id]][[1]], parent = initial, namespace = c())
        terminal <- newXMLNode("terminal", parent = pair, 
                               namespace = c())
        newXMLNode("alternativeID", relations[[id]][[2]], parent = terminal, namespace = c())
        
        values <- newXMLNode("values", parent = pair, namespace = c())
        if (length(reducts.by.relations[[id]]) > 0) {
          for (reduct in reducts.by.relations[[id]]) {
            val <- newXMLNode("value", parent = values, namespace = c())
            newXMLNode("label", reduct, parent = val, namespace = c())  
          }  
        }
      })
      if (inherits(tmpErr, "try-error")) {
        return(list(status="ERROR", errFile = "Impossible to put (a) value(s) in a <alternativesComparisons>."))
      }
    }
  }
  
  return(list(status="OK", errFile=NULL))
}


getExtremeRanksFromXmcdaFile <- function(
  worst.ranking.filename, best.ranking.filename, performances) {
  
  treeWorstRanks <- NULL
  treeBestRanks <- NULL
  worstRanks <- NULL
  bestRanks <- NULL
  ranks <- NULL
  tmpErr <- try(
    treeWorstRanks<-xmlTreeParse(worst.ranking.filename,useInternalNodes=TRUE)
  )
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read worst-ranking.xml file.",
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(treeWorstRanks) == 0) {
    return(list(status="ERROR", errFile ="Worst ranking file is not XMCDA valid.",
                errData=NULL, data=NULL)) 
  }
  
  tmpErr <- try(
    treeBestRanks<-xmlTreeParse(best.ranking.filename,useInternalNodes=TRUE)
  )
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read best-ranking.xml file.",
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(treeBestRanks) == 0) {
    return(list(status="ERROR", errFile ="Best ranking file is not XMCDA valid.",
                errData=NULL, data=NULL)) 
  }
  alternativesIds = rownames(performances)
  flag <- TRUE
  worstRanksFrame <- getAlternativesValues(treeWorstRanks, alternativesIDs=alternativesIds)
  bestRanksFrame <- NULL
  if (worstRanksFrame$status == "OK") {
    worstRanks <- worstRanksFrame[[1]] 
    bestRanksFrame <- getAlternativesValues(treeBestRanks, alternativesIDs=alternativesIds)
    if (bestRanksFrame$status == "OK") {
      bestRanks <- bestRanksFrame[[1]]
      ranks <- cbind(worstRanks[,2], bestRanks[,2])
    } else {
      return(list(status="ERROR", errFile =bestRanksFrame$status,
                errData=NULL, data=NULL)) 
    }
  } else {
    return(list(status="ERROR", errFile =worstRanksFrame$status,
                errData=NULL, data=NULL)) 
  }
  return(list(status="OK", errFile = NULL, errData=NULL, data = ranks))
}


putAlternativesValuesWithReductsData <- function (tree, reducts.by.alternatives, attributes=c()) 
{
  out <- list()
  err1 <- NULL
  err2 <- NULL
  root <- NULL
  tmpErr <- try({
    root <- xmlRoot(tree)
  })
  if (inherits(tmpErr, "try-error")) {
    err1 <- "No <xmcda:XMCDA> found."
  }
  if (length(root) != 0) {
    altVals <- newXMLNode("alternativesValues", attrs = attributes, parent = root, namespace = c())
    for (id in names(reducts.by.alternatives)) {
      tmpErr <- try({
        altVal <- newXMLNode("alternativeValue", parent = altVals, namespace = c())
        altId <- newXMLNode("alternativeID", id, parent = altVal, namespace = c())
      
        values <- newXMLNode("values", parent = altVal, namespace = c())
        if (length(reducts.by.alternatives[[id]]) > 0) {
          for (reduct in reducts.by.alternatives[[id]]) {
            val <- newXMLNode("value", parent = values, namespace = c())
            newXMLNode("label", reduct, parent = val, namespace = c())  
          }  
        }
      })
      if (inherits(tmpErr, "try-error")) {
        return(list(status="ERROR", errFile = "Impossible to put (a) value(s) in a <alternativesValues>."))
      }
    }
  }
  
  return(list(status="OK", errFile=NULL))
}



putAlternativesValuesWithAttributes <- function(tree, alternativesValues, alternativesIDs, attributes=c(), typeOfValues="integer"){
  out<-list()
  err1<-NULL
  err2<-NULL
  root<-NULL
  tmpErr<-try(
    {
      root<-xmlRoot(tree)
    }
  )
  if (inherits(tmpErr, 'try-error')){
    return(list(status="ERROR", errFile = "No <xmcda:XMCDA> found."))
  }
  
  if (length(root)!=0){
    altVals<-newXMLNode("alternativesValues", attrs = attributes, parent=root, namespace=c())
    for (i in 1:dim(alternativesValues)[1]){
      tmpErr<-try(
        {
          altVal<-newXMLNode("alternativeValue", parent=altVals, namespace=c())
          newXMLNode("alternativeID", alternativesIDs[as.numeric(alternativesValues[i,1])], parent = altVal, namespace=c())
          val<-newXMLNode("value", parent = altVal, namespace=c())
          newXMLNode(typeOfValues,alternativesValues[i,2], parent=val, namespace=c())
        }
      )
      if (inherits(tmpErr, 'try-error')){
        return(list(status="ERROR", errFile = "Impossible to put (a) value(s) in a <alternativesValues>."))
      }
    } 
  }
  return(list(status="OK", errFile = NULL))
}


putAlternativesComparisonsWithAttributes <-function(tree, alternativesComparisons, attributes=c()){
  root<-NULL 
  tmpErr<-try(
    {
      root<-xmlRoot(tree)
    }
  )
  if (inherits(tmpErr, 'try-error')){
    return(list(status="ERROR", errFile = "No <xmcda:XMCDA> found."))
  }
  
  if (length(root)!=0){
    altVals<-newXMLNode("alternativesComparisons", attrs = attributes,  parent=root, namespace=c()) 
    pairs<-newXMLNode("pairs", parent=altVals, namespace=c())
    for (i in 1:dim(alternativesComparisons)[1]){
      tmpErr<-try(
        {
          pair<-newXMLNode("pair", parent=pairs, namespace=c())
          initial<-newXMLNode("initial", parent=pair, namespace=c())
          newXMLNode("alternativeID", alternativesComparisons[i,1], parent = initial, namespace=c())
          terminal<-newXMLNode("terminal", parent=pair, namespace=c())
          newXMLNode("alternativeID", alternativesComparisons[i,2], parent = terminal, namespace=c())
          if (dim(alternativesComparisons)[2] > 2)
          {
            val<-newXMLNode("value", parent = pair, namespace=c())
            if (is.na(alternativesComparisons[i,3])){
              newXMLNode("NA",alternativesComparisons[i,3], parent=val, namespace=c())
            }
            else
              newXMLNode("real",alternativesComparisons[i,3], parent=val, namespace=c())
          }
        }
      )
      if (inherits(tmpErr, 'try-error')){
        return(list(status="ERROR", errFile = "Impossible to put (a) value(s) in a <alternativesComparisons>."))
      }
    } 
  }
  return(list(status="OK", errFile = NULL))
}

getAlternativesComparisonsGroupedByIds <- function(tree, alternatives.ids=NULL){
  alternativesComparisons <- getNodeSet(tree, paste("//alternativesComparisons",sep=""))
  results <- list()
  if (length(alternativesComparisons)>0){
    for (i in 1:length(alternativesComparisons)){
      alternativesComp <- matrix(nrow=0,ncol=2)
      id <- xmlGetAttr(alternativesComparisons[[i]], "id")
      pairs <- getNodeSet(alternativesComparisons[[i]], "pairs/pair")
      if (length(pairs)>0){
        for (j in 1:length(pairs)){
          head<-NULL
          tail<-NULL
          tmpErr<-try(
            {
              initial <- getNodeSet(pairs[[j]], "initial/alternativeID")
              terminal <- getNodeSet(pairs[[j]], "terminal/alternativeID")
            }
          )
          if (inherits(tmpErr, 'try-error')){
            return(list(status="ERROR", errFile = "Impossible to read (a) value(s) in a <alternativesComparisons>."))
          }
          
          if (((xmlValue(initial[[1]])%in%alternatives.ids)&(xmlValue(terminal[[1]])%in%alternatives.ids))|(is.null(alternatives.ids))) {
            alternativesComp <-  rbind(alternativesComp,c(xmlValue(initial[[1]]),xmlValue(terminal[[1]])))
          } 
        } 

      } 
      if (dim(alternativesComp)[1] == 0){
        alternativesComp <- NULL
      }
      results[[id]] <- alternativesComp
    } 
  } 
  else { 
    return(list(status="ERROR", errFile = "No <alternativesComparisons> found."))
  }
  
  return(list(status="OK", errFile = NULL, data = results))
}


getAlternativesValuesGroupedByIds <- function(tree, alternativesIDs){
  alternativesValues <- getNodeSet(tree, paste("//alternativesValues",sep=""))
  results <- list()
  if (length(alternativesValues)>0){
    for (i in 1:length(alternativesValues)){
      id <- xmlGetAttr(alternativesValues[[i]], "id")
      results[[id]] <- NULL
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
      if (inherits(tmpErr, 'try-error')){
        return(list(status="ERROR", errFile ="Impossible to read a values in a <alternativesValues>."))
      }
      if (!("alternativesSet" %in% test1.names)& !("interval" %in% test2.names)){     
        altVal <- matrix(nrow=0,ncol=2)
        vals <- getNodeSet(alternativesValues[[i]], "alternativeValue")
        if (length(vals)>0){
          for (j in 1:length(vals)){
            tmpErr<-try(
              {
                alternativeID <- getNodeSet(vals[[j]], "alternativeID")
                val <- getNodeSet(vals[[j]], "value")
                if (length(which(alternativesIDs==xmlValue(alternativeID[[1]])))>0)
                  altVal <-rbind(altVal,c(which(alternativesIDs==xmlValue(alternativeID[[1]])),getNumericValue(val)))
              }
            )
            if (inherits(tmpErr, 'try-error')){
              return(list(status="ERROR", errFile ="Impossible to read a values in a <alternativesValues>."))
            }
          }

        } 
        if (dim(altVal)[1] == 0) {
          altVal <- NULL
        }
        results[[id]] <- altVal
     }
    }
  }
  else {
    return(list(status="ERROR", errFile = "No <alternativesValues> found."))
  }
  return(list(status="OK", errFile=NULL, data=results))
}


getNecessaryRelationsFromXmcdaFile <- function( nec.relations.filename, performances) {
  tree <- NULL
  nec.relations <- NULL
  tmpErr <- try(
    tree<-xmlTreeParse(nec.relations.filename,useInternalNodes=TRUE)
  )
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read necessary relations file.",
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(tree) == 0) {
    return(list(status="ERROR", errFile ="Necessary relations file is not XMCDA valid.",
                errData=NULL, data=NULL)) 
  }
  
  alternatives.ids = rownames(performances)
  num.of.alt = length(alternatives.ids)
  flag <- TRUE
  nec.relations <- getAlternativesComparisonsGroupedByIds(tree, alternatives.ids=alternatives.ids)
  if (nec.relations$status!="OK") {
    return(list(status="ERROR", errFile =nec.relations$errFile,
                errData=NULL, data=NULL)) 
  }
  nec.relations.matrixes <- list()
  for (id in names(nec.relations$data)) {
    nec.relations.matrix <- matrix(data=FALSE, ncol=num.of.alt, nrow=num.of.alt, 
      dimnames=list(alternatives.ids, alternatives.ids))
    for (i in seq(nrow(nec.relations$data[[id]]))) {
      relations <- nec.relations$data[[id]][i, ]
      i1 <- relations[[1]]
      i2 <- relations[[2]]
      nec.relations.matrix[i1, i2] = TRUE
    }
     nec.relations.matrixes[[id]] = nec.relations.matrix
  }
  return(list(status="OK", errFile = NULL, errData=NULL, data = nec.relations.matrixes))
}

getExtremeRanksByNodesFromXmcdaFile <- function(
  worst.ranking.hierarchical.filename, best.ranking.hierarchical.filename, performances) {
  treeWorstRanks <- NULL
  treeBestRanks <- NULL
  worstRanks <- NULL
  bestRanks <- NULL
  results <- list()
  tmpErr <- try(
    treeWorstRanks<-xmlTreeParse(worst.ranking.hierarchical.filename,useInternalNodes=TRUE)
  )
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read worst-ranking-hierarchical.xml file.",
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(treeWorstRanks) == 0) {
    return(list(status="ERROR", errFile ="Worst ranking hierarchical file is not XMCDA valid.",
                errData=NULL, data=NULL)) 
  }
  
  tmpErr <- try(
    treeBestRanks<-xmlTreeParse(best.ranking.hierarchical.filename,useInternalNodes=TRUE)
  )
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read best-ranking-hierarchical.xml file.",
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(treeBestRanks) == 0) {
    return(list(status="ERROR", errFile ="Best ranking hierarchical file is not XMCDA valid.",
                errData=NULL, data=NULL)) 
  }
  alternativesIds = rownames(performances)
  flag <- TRUE
  worstRanksFrame <- getAlternativesValuesGroupedByIds(treeWorstRanks, alternativesIDs=alternativesIds)
  bestRanksFrame <- NULL
  if (worstRanksFrame$status == "OK") {
    worstRanks <- worstRanksFrame$data 
    bestRanksFrame <- getAlternativesValuesGroupedByIds(treeBestRanks, alternativesIDs=alternativesIds)
    if (bestRanksFrame$status == "OK") {
      bestRanks <- bestRanksFrame$data
      for(nodeid in names(bestRanks)) {
        ranks <- NULL
        if (nodeid %in% names(worstRanks)) {
          ranks <- cbind(worstRanks[[nodeid]][,2], bestRanks[[nodeid]][,2])  
        }
        results[[nodeid]] <- ranks
      }
    } else {
      return(list(status="ERROR", errFile =bestRanksFrame$errFile,
                errData=NULL, data=NULL)) 
    }
  } else {
    return(list(status="ERROR", errFile =worstRanksFrame$errFile,
                errData=NULL, data=NULL)) 
  }
  return(list(status="OK", errFile = NULL, errData=NULL, data = results))
}

getAlternativesComparisonFromXmcdaFile <- function(filename, performances){
  tree <- NULL
  tmpErr<-try(
    {
      tree<-xmlTreeParse(filename,useInternalNodes=TRUE)
    }, silent=TRUE
  )  
  if (inherits(tmpErr, 'try-error')) {
    return(list(status="ERROR", errFile ="Cannot read target.xml file.", 
                errData=NULL, data=NULL)) 
  }
  if (checkXSD(tree)==0) {
    return(list(status="ERROR", errFile = "Target file is not XMCDA valid.",
                errData=NULL, data=NULL))  
  }
  relations <- getAlternativesComparisonsLabels(tree, rownames(performances))
  if (relations$status == "OK") {
    return(list(status="OK", errFile=NULL,
                errData=NULL, data=relations[[1]][1,]))
  } else {
    return(list(status="ERROR", errFile=NULL,
                errData=relations$status, data=NULL))  
  }
}
