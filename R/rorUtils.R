
#assignInNamespace("MINEPS", ns="ror", value=10e-10)
MINEPS <- 0.005
#assignInNamespace("MINEPS", ns="ror", value=10e-10)
M_BIG_NUMBER <- 99

#library(ror)

#library(rJava)

#Function returns a vector with boolean values. False - criterion is used.
getCriteriaFilter <- function(perf, criteria=NULL) {
  levels <- getLevels(perf)
  nrVars <- getNrVars(levels)
  if (is.null(criteria)) {
    return(matrix(data=FALSE, nrow=1,ncol=nrVars))
  }

  filter = matrix(data=TRUE, nrow=1,ncol=nrVars)
  filter[1, nrVars] <- FALSE
  offsets <- getOffsets(levels)
  criteria.names <- colnames(perf)
  #for each criterion id
  for (criterionID in criteria) {
    index <- which(criteria.names == criterionID)
    if (length(index) == 1) {
      start.index <- offsets[index]
      last.index <- length(filter)-1
      if (index < length(offsets)) {
          last.index <- offsets[index+1]-1
      }
      for (i in seq(start.index, last.index)) {
        filter[i] <- FALSE
      }  
    }
  }
  return(filter)
}

getFilteredAltVariableMatrix <- function(altVar, filter) {  
  altVar[,filter] <- 0
  return(altVar)
} 


getLastElement <- function(vector) {
  tail(vector, n=1)
}

combineConstraints <- function(...) {
  allConst = list(...)  
  lhs <- c()
  dir <- c()
  rhs <- c()
  for (const in allConst) {
    lhs <- rbind(lhs, const$lhs)
    dir <- c(dir, const$dir)
    rhs <- c(rhs, const$rhs)
  }
  return(list(lhs=lhs, dir=dir, rhs=rhs))
}

getLevels <- function(perf) {
  res <- list()
  for (i in 1:ncol(perf)) {
    res[[i]] <- sort(unique(perf[,i]))
  }
  return(res)
}

getNrVars <- function(levels) {
  return(sum(as.numeric(lapply(levels, length))) + 1)
}

getOffsets <- function(levels) {
  x <- cumsum(lapply(levels, length))
  return(c(1, x[1:length(x)-1] + 1))
}

getNumberOfValues <- function(perf) {
  levels <- getLevels(perf);
  nr.vars <- getNrVars(levels);
  return(nr.vars)
}

buildAltVariableMatrix <- function(perf) {
  levels <- getLevels(perf)
  offsets <- getOffsets(levels)
  nrAlts <- nrow(perf)
  nrCrit <- ncol(perf)
  nrVars <- getNrVars(levels)
  resMat = matrix(nrow=nrAlts,ncol=nrVars, dimnames=c(dimnames(perf)[1]))
  for (i in seq(1:nrAlts)) {
    vec <- array(0, dim=nrVars)
    indices <- sapply(seq(1:nrCrit), function(x) {which(levels[[x]] == perf[i,x])})
    vec[indices + offsets - 1] = 1
    resMat[i,] = vec
  }
  return(resMat)
}

buildStrongPreferenceConstraint <- function(a, b, altVars) {
  nrVars <- dim(altVars)[2]
  lhs <- altVars[a,]
  lhs[length(lhs)] = -1
  lhs <- lhs - altVars[b,]
  return(list(lhs=lhs, dir=">=", rhs=0))
}

filterAltVars <- function(altVars, offsets, criterion.id) {
  ind1 = offsets[criterion.id]
  ind2 = ncol(altVars)
  if (criterion.id < length(offsets)) {
    ind2 = offsets[criterion.id + 1] - 1
  }
  vars <- matrix(0, ncol=ncol(altVars), nrow=nrow(altVars))
  vars[,ind1:ind2] = altVars[,ind1:ind2]
  return(vars)
}

buildIntensityOfPreferenceStrongConstraint <- function(a, b, c, d,  altVars) {
  vars <- altVars
 
  lhs <- vars[a,]
  lhs[length(lhs)] = -1
  lhs <- lhs + vars[d,] - vars[b,] - vars[c,]
  return(list(lhs=lhs, dir=">=", rhs=0))
}

buildIndifPreferenceConstraint <- function(a, b, altVars) {
  lhs <- altVars[a,]
  lhs <- lhs - altVars[b,]
  return(list(lhs=lhs, dir="==", rhs=0))
}

buildIntensityOfPreferenceIndifConstraint <- function(a, b, c, d,  altVars) {
  vars = altVars
  #if (criterion.id >= 1) {
  #  vars <- FilterAltVars(altVars, offsets, criterion.id)
  #}
  lhs <- vars[a,]
  lhs <- lhs + vars[d,] - vars[b,] - vars[c,]
  return(list(lhs=lhs, dir="==", rhs=0))
}

buildWeakPreferenceConstraint <- function(a, b, altVars) {
  lhs <- altVars[a,]
  lhs <- lhs - altVars[b,]
  
  return(list(lhs=lhs, dir=">=", rhs=0))
}

buildIntensityOfPreferenceWeakConstraint <- function(a, b, c, d,  altVars) {
  vars = altVars
  #if (criterion.id >= 1) {
  #vars <- FilterAltVars(altVars, offsets, criterion.id)
  #}
  lhs <- vars[a,]
  lhs <- lhs + vars[d,] - vars[b,] - vars[c,]
  return(list(lhs=lhs, dir=">=", rhs=0))
}


getNumberOfVariables <- function(perf, numbers.of.characteristic.points){
  # Function returns number of variables used in problem
  #
  # perf - matrix of options and their ratings on criteria
  # numbers.of.characteristic.points - sequence of numbers. Position i of this array means number of characteristic
  #                        points, on i - criteria
  # return - number of variables
  levels <- getLevels(perf);
  num.of.values = getNrVars(levels)
  num.of.characteristic.points = sum(numbers.of.characteristic.points);
  num.of.variables = num.of.values + num.of.characteristic.points;
  return(num.of.variables);
}

getIntervalData <- function(characteristic.points, criterion.index, value) {
  i = 2;
  while((characteristic.points[[criterion.index]][i] < value) & (i <= length(characteristic.points[[criterion.index]]))) {
    i = i + 1;
  }
  
  bottom = 0
  top = 0
  if (i <= length(characteristic.points[[criterion.index]])) {
    bottom = characteristic.points[[criterion.index]][i-1];
    top = characteristic.points[[criterion.index]][i];
  }
  interval = list(index=i, value = c(bottom, top));
  return(interval)
} 

getCharacteristicPoints <- function(perf, nums.of.characteristic.points) {
  # Function return list of lists of characteristic points for each criterion
  # 
  # perf: the performance matrix
  # nums.of.characteristic.points: list of nums of characteristic points for each criterion 
  levels.list <- getLevels(perf);
  list.of.characteristic.points = list()
  nrAlts <- nrow(perf)
  nrCrit <- ncol(perf)
  for (i in c(1:nrCrit)){  #dla każdego kryterium
    list.of.characteristic.points[[i]] = vector(mode="numeric", length=0)
    num.of.characteristic.points = nums.of.characteristic.points[i]
    if (!is.numeric(num.of.characteristic.points)) {
      num.of.characteristic.points = 0
    }
    if (num.of.characteristic.points > 1) {
      for (j in c(1:num.of.characteristic.points)) {
        last.level = getLastElement(levels.list[[i]])      
        range.of.performance = last.level - levels.list[[i]][1]
        characteristic.point = levels.list[[i]][1] + (range.of.performance)*(j-1)/(num.of.characteristic.points-1) 
        list.of.characteristic.points[[i]] = c(list.of.characteristic.points[[i]], characteristic.point)
      }  
    } else {
      list.of.characteristic.points[[i]] = list()
    }
  }
  return(list.of.characteristic.points);
}


buildMonotonousConstraintsForValuesOrCharacteristicPoints <- function(perf,
                                                              characteristic.points, offsets,  offsets.for.characteristic.points, num.of.variables, strict.vf = FALSE) {
  levels.list <- getLevels(perf)  
  left.side.of.constraints = c()
  for (i in seq(1:length(characteristic.points))) {  
    if (length(characteristic.points[[i]]) > 0) {
      for (j in seq(1:(length(characteristic.points[[i]]) - 1))) {
        index <- offsets.for.characteristic.points[i] + j - 1
        #list of variables, first numOfValues variables means g(x) and epsilon. Next numOfCharacteristicPoints
        #values means g(q_j) and epsilon
        piece.lin.monotonic = vector(mode = "numeric", length = num.of.variables)
        piece.lin.monotonic[index] <- 1
        piece.lin.monotonic[index+1] <- -1
        if (strict.vf == TRUE) {
          nrVars <- getNrVars(levels.list);
          piece.lin.monotonic[nrVars] = 1;
        }
        left.side.of.constraints <- rbind(left.side.of.constraints, piece.lin.monotonic)
      }
    } else {
      res <- c()
      
      if (length(levels.list[[i]]) >= 2) {
        for (j in seq(1:(length(levels.list[[i]])-1))) {
          
          index <- offsets[i] + j - 1
          monotonic <- array(0, dim=num.of.variables)
          monotonic[index] <- 1
          monotonic[index+1] <- -1
          if (strict.vf == TRUE) {
            nrVars <- getNrVars(levels.list);
            monotonic[nrVars] = 1;
          }
        
          res <- rbind(res, monotonic)
        }
      }
      
      left.side.of.constraints <- rbind(left.side.of.constraints, res)
    }
  }
  result <- list(lhs=left.side.of.constraints, dir=rep("<=", nrow(left.side.of.constraints)),
                 rhs=rep(0.0, nrow(left.side.of.constraints)))
  return(result)
}

buildConstraintsForValuesEvaluation <- function(characteristic.points, 
                                                offsets.for.characteristic.points, levels.list, numOfVariables) {
  offsets.levels = getOffsets(levels.list);
  left.side.of.constraints = c()
  for (i in seq(1:length(levels.list))) {
    if (length(characteristic.points[[i]] > 0)) {  
      for (j in seq(1:length(levels.list[[i]]))) {
        interval <- getIntervalData(characteristic.points, i, levels.list[[i]][j])
        proc = (levels.list[[i]][j] - interval$value[[1]]) / (interval$value[[2]] - interval$value[[1]])
        piece.lin.values = vector(mode="numeric", length=numOfVariables)
        index.levels <- offsets.levels[i] + j - 1;
        index.interval <- offsets.for.characteristic.points[i] + interval$index - 1; 
        piece.lin.values[index.levels] = -1;
        piece.lin.values[index.interval-1] = 1-proc;
        piece.lin.values[index.interval] = proc;
        left.side.of.constraints <- rbind(left.side.of.constraints, piece.lin.values)
      }
    }
  }
  if (!is.null(left.side.of.constraints)) {
    return(list(lhs=left.side.of.constraints, dir=rep("==", nrow(left.side.of.constraints)), rhs=rep(0.0, nrow(left.side.of.constraints))))
  }
  return(list())
}

buildMonotonousConstraints <- function(perf, strict.vf=FALSE, nums.of.characteristic.points=c()) {
  
  stopifnot(is.logical(strict.vf));
  levels.list <- getLevels(perf);
  characteristic.points = getCharacteristicPoints(perf, nums.of.characteristic.points);
  num.of.values = getNrVars(levels.list)
  num.of.characteristic.points = getNrVars(characteristic.points) -1;
  offsets <- getOffsets(levels.list);
  offsets.for.characteristic.points <- getOffsets(characteristic.points);
  offsets.for.characteristic.points = offsets.for.characteristic.points + num.of.values
  num.of.variables = num.of.values + num.of.characteristic.points;
  c1 <- buildMonotonousConstraintsForValuesOrCharacteristicPoints(
    perf, characteristic.points, offsets,  offsets.for.characteristic.points, num.of.variables, strict.vf)
  c2 <- buildConstraintsForValuesEvaluation(
    characteristic.points, offsets.for.characteristic.points, levels.list, num.of.variables)
  monotonousConst <- combineConstraints(c1, c2)
  return(monotonousConst)
}

buildFirstLevelZeroConstraints <- function(perf) {
  levels <- getLevels(perf)
  offsets <- getOffsets(levels)
  nrVars <- getNrVars(levels)
  res <- matrix(0, nrow=length(offsets),ncol=nrVars)
  for (i in seq(1:length(offsets))) {
    res[i,offsets[i]] = 1
  }
  rownames(res) <- rep('lowest.equal.zero',nrow(res))
  return(list(lhs=res,dir=rep("==", length(offsets)),rhs=rep(0,length(offsets))))
}

buildBestLevelsAddToUnityConstraint <- function(perf) {
  levels <- getLevels(perf)
  offsets <- getOffsets(levels)
  nrVars <- getNrVars(levels)
  lhs <- c()
  best.add.to.one <- rep(0, nrVars)
  ind <- c((offsets-1)[-1], nrVars-1)
  best.add.to.one[ind] = 1
  lhs <- rbind(lhs, best.add.to.one)
  return(list(lhs=lhs, dir="==", rhs=1))
}

buildAllVariablesLessThan1Constraint <- function(perf) {
  levels <- getLevels(perf)
  nrVars <- getNrVars(levels)
  lhs <- diag(nrVars)
  rownames(lhs) <- rep('less.than.one',nrow(lhs))
  return(list(lhs=lhs, dir=rep("<=", nrVars), rhs=rep(1, nrVars)))
}

buildEpsilonStrictlyPositiveConstraint <- function(perf) {
  levels <- getLevels(perf)
  nrVars <- getNrVars(levels)
  eps <- rep(0, nrVars)
  eps[length(eps)] = 1
  return(list(lhs=rbind(c(), eps), dir=">=", rhs=MINEPS))
}

getNormalizedMatrix <- function(matrix, width, right=TRUE) {
  #Function adds empty columns if width of matrix is to small
  # matrix - matrix
  # witdth - expected width of matrix
  # return normalized matrix
  if (!is.matrix(matrix) &&(!is.null(matrix))) {
    matrix = matrix(matrix, nrow=1)  
  }
  if (length(matrix) == 0) {
    return(NULL)
  }
 
  if (ncol(matrix) < width) {
    numOfAdditionalCols <- width - ncol(matrix);
    tempMatrix <- matrix(0.0, ncol=numOfAdditionalCols, nrow(matrix));
    if (right == TRUE) {
      matrix <- cbind(matrix, tempMatrix);
    } else {
      matrix <- cbind(tempMatrix, matrix);
    }
  }
  return(matrix);
}

buildBaseConstraints <- function( perf, num.of.variables, strict.vf = FALSE,  nums.of.characteristic.points = NULL) {
  
  # perf - performances matrix
  # num.of.variables - number of variables used in problem
  # strict.vf - TRUE -> value functions strictly increasing (instead of monotonous increasing)
  # is.piecewise.linear - Boolean variable determines whether functions should 
  #    be piecewise linear
  # nums.of.characteristicPoints - list of values - each value means num of characteristic points on criterium
  c = list(); #list of constraints
  c[[1]] <- buildMonotonousConstraints(perf, strict.vf, nums.of.characteristic.points)  
  c[[2]] <- buildFirstLevelZeroConstraints(perf)
  c[[3]] <- buildBestLevelsAddToUnityConstraint(perf)
  c[[4]] <- buildAllVariablesLessThan1Constraint(perf)
  c[[5]] <- buildEpsilonStrictlyPositiveConstraint(perf)
  normalized.c = list();
  for (constraint in c) {
    constraint$lhs <- getNormalizedMatrix(constraint$lhs, num.of.variables)
    normalized.c[length(normalized.c)+1] <- list(constraint)
  }
  base.constraints <- combineConstraints(normalized.c[[1]], normalized.c[[2]], normalized.c[[3]], normalized.c[[4]], normalized.c[[5]])
  return(base.constraints)
}

buildPairwiseComparisonConstraints <- function(perf, strong.prefs = NULL,
                                               weak.prefs = NULL, indif.prefs = NULL, criteria.by.nodes=NULL) {
  #Function builds constraints for pairwise comparison 
  alt.vars <- buildAltVariableMatrix(perf)
  all.constraints <- NULL
  if (is.matrix(strong.prefs)) {
    for (i in 1:nrow(strong.prefs)) {
      selected.criteria <- NULL
      group.name <- NULL
      if (ncol(strong.prefs) > 2) {
        selected.criteria <- criteria.by.nodes[[strong.prefs[i,3]]]
        group.name <- paste("[", strong.prefs[i,3], "]", sep="")
      }
      pref.constraints <- buildStrongPreferenceConstraint(strong.prefs[i,1], strong.prefs[i,2], getFilteredAltVariableMatrix(alt.vars, getCriteriaFilter(perf,selected.criteria)))
      all.constraints <- combineConstraints(all.constraints, pref.constraints);
      row.names(all.constraints$lhs)[nrow(all.constraints$lhs)] <- c(paste(paste(strong.prefs[i,1], ">", strong.prefs[i,2]), group.name, sep=""))
    }
  }
  
  if (is.matrix(weak.prefs)) {
    for (i in 1:nrow(weak.prefs)) {
      selected.criteria <- NULL
      group.name <- NULL
      if (ncol(weak.prefs) > 2) {
        selected.criteria <- criteria.by.nodes[[weak.prefs[i,3]]] 
        group.name <- paste("[", weak.prefs[i,3], "]", sep="")
      }
      pref.constraints <- buildWeakPreferenceConstraint(weak.prefs[i,1], weak.prefs[i,2], getFilteredAltVariableMatrix(alt.vars, getCriteriaFilter(perf,selected.criteria)))
      all.constraints <- combineConstraints(all.constraints, pref.constraints);
      row.names(all.constraints$lhs)[nrow(all.constraints$lhs)] <- c(paste(paste(weak.prefs[i,1], ">=", weak.prefs[i,2]), group.name, sep=""))
    }
  }
  if (is.matrix(indif.prefs)) {
    for (i in 1:nrow(indif.prefs)) {
      selected.criteria <- NULL
      group.name <- NULL
      if (ncol(indif.prefs) > 2) {
        selected.criteria <- criteria.by.nodes[[indif.prefs[i,3]]]
        group.name <- paste("[", indif.prefs[i,3], "]", sep="")
      }
      pref.constraints <- buildIndifPreferenceConstraint(indif.prefs[i,1], indif.prefs[i,2], getFilteredAltVariableMatrix(alt.vars, getCriteriaFilter(perf,selected.criteria)))
      all.constraints <- combineConstraints(all.constraints, pref.constraints);
      row.names(all.constraints$lhs)[nrow(all.constraints$lhs)] <- c(paste(paste(indif.prefs[i,1], "==", indif.prefs[i,2]), group.name, sep=""))
    }
  }
  return(all.constraints)  
} 

buildIntensitiesOfPreferenceConstraints <- function(perf, strong.intensities.of.prefs = NULL, 
                                                    weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, criteria.by.nodes=NULL) {
  #Function builds constraints for pairwise comparison 
  alt.vars <- buildAltVariableMatrix(perf)
  all.constraints <- NULL
  levels = getLevels(perf=perf)
  offsets = getOffsets(levels=levels)
  
  if (is.matrix(strong.intensities.of.prefs)) {
    for (i in 1:nrow(strong.intensities.of.prefs)) {
      selected.criteria <- NULL
      if  (strong.intensities.of.prefs[i, 5] != 0) {
        if (!is.null(criteria.by.nodes) && (strong.intensities.of.prefs[i,5] %in% names(criteria.by.nodes))) {
          selected.criteria <- criteria.by.nodes[[strong.intensities.of.prefs[i,5]]]
        } else {
          selected.criteria <- list(strong.intensities.of.prefs[i,5])  
        }
      }
      
      pref.constraints <- buildIntensityOfPreferenceStrongConstraint(strong.intensities.of.prefs[i,1],
                                                                     strong.intensities.of.prefs[i,2],
                                                                     strong.intensities.of.prefs[i,3],
                                                                     strong.intensities.of.prefs[i,4],
                                                                     altVars=getFilteredAltVariableMatrix(alt.vars, getCriteriaFilter(perf,selected.criteria)))
      all.constraints <- combineConstraints(all.constraints, pref.constraints);
      sign <- ">("
      if (strong.intensities.of.prefs[i, 5] != 0) {
        sign <- paste(">[",strong.intensities.of.prefs[i, 5],"](",sep='')
      }
      row.names(all.constraints$lhs)[nrow(all.constraints$lhs)] <- c(
        paste('(',strong.intensities.of.prefs[i,1],',',strong.intensities.of.prefs[i,2],')', sign, 
              strong.intensities.of.prefs[i,3], ',', strong.intensities.of.prefs[i,4], ')', sep=''))
    }
  }
  
  if (is.matrix(weak.intensities.of.prefs)) {
    for (i in 1:nrow(weak.intensities.of.prefs)) {
      selected.criteria <- NULL
      if  (weak.intensities.of.prefs[i, 5] != 0) {
        if (!is.null(criteria.by.nodes) && (weak.intensities.of.prefs[i,5] %in% names(criteria.by.nodes))) {
          selected.criteria <- criteria.by.nodes[[weak.intensities.of.prefs[i,5]]]  
        } else {
          selected.criteria <- list(weak.intensities.of.prefs[i,5])  
        }
      }
      pref.constraints <- buildIntensityOfPreferenceWeakConstraint(weak.intensities.of.prefs[i,1],
                                                                     weak.intensities.of.prefs[i,2],
                                                                     weak.intensities.of.prefs[i,3],
                                                                     weak.intensities.of.prefs[i,4],
                                                                     altVars=getFilteredAltVariableMatrix(alt.vars, getCriteriaFilter(perf,selected.criteria)))
      all.constraints <- combineConstraints(all.constraints, pref.constraints);
      sign <- ">=("
      if (weak.intensities.of.prefs[i, 5] > 0) {
        sign <- paste(">=[",weak.intensities.of.prefs[i, 5],"](",sep='')
      }
      row.names(all.constraints$lhs)[nrow(all.constraints$lhs)] <- c(
        paste('(',weak.intensities.of.prefs[i,1],',',weak.intensities.of.prefs[i,2],')', sign, 
              weak.intensities.of.prefs[i,3], ',', weak.intensities.of.prefs[i,4], ')', sep=''))
    }
  }
  if (is.matrix(indif.intensities.of.prefs)) {
    for (i in 1:nrow(indif.intensities.of.prefs)) {
      selected.criteria <- NULL
      if  (indif.intensities.of.prefs[i, 5] != 0) {
        if (!is.null(criteria.by.nodes) && (indif.intensities.of.prefs[i,5] %in% names(criteria.by.nodes))) {
          selected.criteria <- criteria.by.nodes[[indif.intensities.of.prefs[i,5]]]  
        } else {
          selected.criteria <- list(indif.intensities.of.prefs[i,5])  
        }
      }
      pref.constraints <- buildIntensityOfPreferenceIndifConstraint(indif.intensities.of.prefs[i,1],
                                                                   indif.intensities.of.prefs[i,2],
                                                                   indif.intensities.of.prefs[i,3],
                                                                   indif.intensities.of.prefs[i,4],
                                                                   altVars=getFilteredAltVariableMatrix(alt.vars, getCriteriaFilter(perf,selected.criteria)))
      all.constraints <- combineConstraints(all.constraints, pref.constraints);
      sign <-"==("
      if (indif.intensities.of.prefs[i, 5] > 0) {
        sign <- paste("==[",indif.intensities.of.prefs[i, 5],"](",sep='')
      }
      row.names(all.constraints$lhs)[nrow(all.constraints$lhs)] <- c(
        paste('(',indif.intensities.of.prefs[i,1],',',indif.intensities.of.prefs[i,2],')', sign, 
              indif.intensities.of.prefs[i,3], ',', indif.intensities.of.prefs[i,4], ')', sep=''))
    }
  }
  return(all.constraints)  
}  

buildVariantsDiffMatrix <- function(alt.vars, index.of.variant.1, index.of.variant.2) {
  mat <- alt.vars[index.of.variant.1,]
  mat <- mat - alt.vars[index.of.variant.2,]
  mat <- matrix(data = mat, nrow = 1)
  row.names(mat) <- paste(index.of.variant.1, "-", index.of.variant.2)
  return(mat)
}

buildRankRelatedRequirementsConstraints <- function(perf, rank.related.requirements, constraints.width, criteria.by.nodes=NULL){
  if (!is.matrix(rank.related.requirements)) {
    return(NULL)
  } 
  
  constraints.length = constraints.width
  number.of.alternatives <- nrow(perf)
  number.of.binary.variables <- number.of.alternatives 
  
  number.of.requirements <- nrow(rank.related.requirements)
  variants <- seq(number.of.binary.variables)
  constraints <- list()
  lhs <- list()
  dir <- c()
  rhs <- c()
  
  for (index in seq(number.of.requirements)) { #for each requirement
    i <- rank.related.requirements[index, 1]
    if (!is.numeric(i)) {
      i <- which(rownames(perf) == i)   
    }
    p_worst <- as.numeric(rank.related.requirements[index, 2]) 
    p_best <- as.numeric(rank.related.requirements[index, 3])
    set.of.criteria <- NULL
    if ((!is.null(criteria.by.nodes)) && (ncol(rank.related.requirements) > 3)) {
      set.of.criteria <- criteria.by.nodes[[rank.related.requirements[4]]];  
    }
    differencesIJ = c()
    differencesJI = c()
    additional.variables.list.IJ = c()
    additional.variables.list.JI = c()
    additional.variables.binary.sum.list.IJ = c()
    additional.variables.binary.sum.list.JI = c()
    for (j in variants) {
      if (i != j) {        
        alt.vars <- buildAltVariableMatrix(perf)
        filtered.alt.vars <- getFilteredAltVariableMatrix(
                                altVar=alt.vars, filter=getCriteriaFilter(
                                  perf=perf,
                                  criteria=set.of.criteria
                                )
                             )
        num.of.vars <- getNumberOfValues(perf)
        differenceIJ <- buildVariantsDiffMatrix(filtered.alt.vars, i, j)
        differenceJI <- buildVariantsDiffMatrix(filtered.alt.vars, j, i)
        differenceIJ[num.of.vars] = -1
        differenceJI[num.of.vars] = -1
        additional.variablesIJ = vector(mode="numeric",
                                    length=number.of.binary.variables)
        additional.variablesJI = vector(mode="numeric",
                                        length=number.of.binary.variables)
        additional.variables.binary.sumIJ = vector(mode="numeric",
                                        length=number.of.binary.variables)
        additional.variables.binary.sumJI = vector(mode="numeric",
                                        length=number.of.binary.variables)
        additional.variablesIJ[j] = M_BIG_NUMBER
        additional.variablesJI[j] = M_BIG_NUMBER
        additional.variables.binary.sumIJ[j] = 1
        additional.variables.binary.sumJI[j] = 1
        additional.variables.list.IJ = rbind(additional.variables.list.IJ,
                                        additional.variablesIJ)
        additional.variables.list.JI = rbind(additional.variables.list.JI,
                                            additional.variablesJI)
        additional.variables.binary.sum.list.IJ = rbind(additional.variables.binary.sum.list.IJ,
                                             additional.variables.binary.sumIJ)
        additional.variables.binary.sum.list.JI = rbind(additional.variables.binary.sum.list.JI,
                                             additional.variables.binary.sumJI)
        differencesJI <- rbind(differencesJI, differenceJI)
        differencesIJ <- rbind(differencesIJ, differenceIJ)
      }
    }
    
    binary.pair.sum.less.than.one <- cbind(additional.variables.binary.sum.list.IJ, additional.variables.binary.sum.list.JI)
    binary.pair.sum.less.than.one <- getNormalizedMatrix(binary.pair.sum.less.than.one, constraints.length + ncol(binary.pair.sum.less.than.one), right=FALSE)
    rownames(binary.pair.sum.less.than.one) <- rep("bin.pair.sum <= 1", nrow(binary.pair.sum.less.than.one))
    
    builded.constraints <- getNormalizedMatrix(differencesIJ, constraints.length)
    builded.constraints <- cbind(builded.constraints, additional.variables.list.IJ)
    vector.sum.IJ <- rep(1,ncol(additional.variables.list.IJ))
    vector.sum.IJ[i] <- 0
    vector.sum.JI <- rep(1, ncol(additional.variables.list.JI))
    vector.sum.JI[i] <-0
    sum.of.ij.vector <-c(rep(0, constraints.length), vector.sum.IJ, rep(0, ncol(additional.variables.list.JI)))
    sum.of.ji.vector <-c(rep(0, constraints.length), rep(0,ncol(additional.variables.list.IJ)), vector.sum.JI)  
    constraints.length <- ncol(builded.constraints)
    differencesJI <- getNormalizedMatrix(differencesJI, constraints.length)
    differencesJI <- cbind(differencesJI, additional.variables.list.JI)
    constraints.length <- ncol(differencesJI)
    builded.constraints <- getNormalizedMatrix(builded.constraints, constraints.length)
    builded.constraints <- rbind(builded.constraints, differencesJI, sum.of.ij.vector, sum.of.ji.vector, binary.pair.sum.less.than.one)
    lhs <- getNormalizedMatrix(lhs, ncol(builded.constraints))
    
    lhs <- rbind(lhs, builded.constraints)
    constraints.length <- ncol(lhs)
    dir <- c(dir, c(rep(">=", nrow(additional.variables.list.IJ)*2), "<=", "<=", rep("<=", nrow(binary.pair.sum.less.than.one))))
    rhs <- c(rhs, c(rep( 0, nrow(additional.variables.list.IJ)*2)), p_worst - 1 , number.of.alternatives - p_best, c(rep(1, nrow(binary.pair.sum.less.than.one))))
  }
  constraints <- list(
    "lhs" = lhs,
    "dir" = dir,
    "rhs" = rhs
  )
  
  return(constraints)
}




mergeConstraintsIntoBaseLPModel <- function(num.of.variables, base.constraints, pairwise.comparison.constraints=NULL, intensities.of.preferences.constraints=NULL, rank.related.requirements.constraints=NULL) {
  if (!is.null(pairwise.comparison.constraints)) {
    pairwise.comparison.constraints$lhs <- getNormalizedMatrix(pairwise.comparison.constraints$lhs, num.of.variables);  
    base.constraints <- combineConstraints(base.constraints, pairwise.comparison.constraints);    
  }
  
  if (!is.null(intensities.of.preferences.constraints)) {
    intensities.of.preferences.constraints$lhs <- getNormalizedMatrix(intensities.of.preferences.constraints$lhs, num.of.variables);  
    base.constraints <- combineConstraints(base.constraints, intensities.of.preferences.constraints);    
  }
  
  if (!is.null(rank.related.requirements.constraints)) {
    rank.related.requirements.constraints$lhs <- getNormalizedMatrix(rank.related.requirements.constraints$lhs, ncol(base.constraints$lhs));  
    base.constraints$lhs <- getNormalizedMatrix(base.constraints$lhs, ncol(rank.related.requirements.constraints$lhs))
    base.constraints <- combineConstraints(base.constraints, rank.related.requirements.constraints);   
  }
  
  return(base.constraints)
}



#'Function builds and return constraints for base LP model
#'  perf - performances matrix
#'  strict.vf - TRUE -> value functions strictly increasing (instead of monotonous increasing)
#'  *.prefs - an n x 2 matrix, where each row (a, b) means that a is 
#'    [strongly or weakly preferred, or indifferent] to b.
#'  *.intensities.of.prefs - an n x 5 matrix, where each row (a, b, c, d, x) means that preference relation between a and b 
#'    is weakly, strongly preffered or indifferent to relation between c and d. 
#'    x means a criterion which is taken into account (0 means that all criteria should be used).
#'  rank.related.requirements - an n x 3 matrix (row: (id.of.alternative, worst.possible.rank.position, best.possible.rank.position)). 
#'  nums.of.characteristicPoints - list of values - each value means num of characteristic points on criterium
#'  
buildBaseLPModel <- function(perf, 
                             strict.vf, 
                             strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                             strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                             rank.related.requirements = NULL,
                             nums.of.characteristic.points=NULL, criteria.by.nodes=NULL) {
  
  
  if (is.null(nums.of.characteristic.points)) {
    nums.of.characteristic.points = rep(0, ncol(perf))
  }
  
  num.of.variables = getNumberOfVariables(perf, nums.of.characteristic.points)

  all.constraints <- buildBaseConstraints(perf, num.of.variables, strict.vf, nums.of.characteristic.points)
  
  pairwise.comparison.constraints <- buildPairwiseComparisonConstraints(perf,
                                                                        strong.prefs, weak.prefs, indif.prefs,
                                                                        criteria.by.nodes=criteria.by.nodes)
  
  intensities.of.preferences.constraints <- buildIntensitiesOfPreferenceConstraints(perf, strong.intensities.of.prefs,
                                                                       weak.intensities.of.prefs, 
                                                                       indif.intensities.of.prefs, criteria.by.nodes=criteria.by.nodes)
  

  constraints.width = ncol(all.constraints$lhs)
  rank.related.requirements.constraints <- buildRankRelatedRequirementsConstraints(perf, rank.related.requirements, constraints.width, criteria.by.nodes=criteria.by.nodes)
  
  all.constraints <- mergeConstraintsIntoBaseLPModel(num.of.variables = num.of.variables, 
                                                     base.constraints = all.constraints, 
                                                     pairwise.comparison.constraints = pairwise.comparison.constraints, 
                                                     intensities.of.preferences.constraints = intensities.of.preferences.constraints,
                                                     rank.related.requirements.constraints = rank.related.requirements.constraints)
  
  return(all.constraints) 
}

solveModel <- function(model, number.of.real.variables) {
  number.of.all.variables <- ncol(model$lhs)

  types <- rep('C', number.of.all.variables)
  if (number.of.real.variables < number.of.all.variables){
    position.of.first.binary.variable <- number.of.real.variables + 1
    #position.of.first.binary.variable <- number.of.variables - number.of.binary.variables + 1
    types[position.of.first.binary.variable : number.of.all.variables] <- 'B'
  }  
  obj <- L_objective(buildObjectiveFunction(number.of.real.variables, number.of.all.variables))
  roiConst <- L_constraint(L = model$lhs, dir =model$dir, rhs=model$rhs)
  lp <- OP(objective=obj, constraints=roiConst, maximum=TRUE, types=types)
  res <- ROI_solve(lp, .solver)
  return(res)
}

generateAllSubsetsOfConstraints <- function(all.constraints) {
  
  indexes <- lapply(1:nrow(all.constraints$lhs), function(x) combn(nrow(all.constraints$lhs),x))
  all.subsets.of.constraints = list()
  k = 0
  for (n in seq(length(indexes))) {
    for(j in seq(ncol(indexes[[n]]))) {
      constraints <- NULL
      for(i in seq(nrow(indexes[[n]]))) {
        constraints$lhs <- rbind(constraints$lhs, all.constraints$lhs[indexes[[n]][i,j], ])
        constraints$dir <- cbind(constraints$dir, all.constraints$dir[indexes[[n]][i,j]])
        constraints$rhs <- cbind(constraints$rhs, all.constraints$rhs[indexes[[n]][i,j]])
        row.names(constraints$lhs)[i] <- row.names(all.constraints$lhs)[indexes[[n]][i,j]] 
      }
      k <- k + 1 
      
      all.subsets.of.constraints[[k]] <- constraints
      
    }
  }
  
  return(all.subsets.of.constraints)
}


buildObjectiveFunction <- function(number.of.real.variables, number.of.all.variables) {
  objective <- rep(0, number.of.all.variables)
  objective[number.of.real.variables] <- 1  
 
  return(objective)
}

checkConstraintsConsistency <- function(model, number.of.real.variables) {
  #check constraints consistency  
  #  model: structure of constraints with the following elements:
  #    model$lhs: matrix - left side of constraints
  #    model$dir: list of operators
  #    model$rhs: matrix - right side of constraints
  ret <- solveModel( model=model, number.of.real.variables=number.of.real.variables)
  return(ret$status$code == 0 && ret$objval >= MINEPS)
}



findNecessaryOrPossibleRelations <- function(performances, base.model, number.of.real.variables, necessary=TRUE, filter=NULL) {
  rel <- matrix(nrow=nrow(performances), ncol=nrow(performances))
    
  for (i in 1:nrow(rel)) {
    for(j in 1:nrow(rel)) {
      rel[i,j] = checkRelation(perf=performances, base.model = base.model, number.of.real.variables, a = i, b = j, necessary = necessary, filter=filter)
    }
  }
  if (!is.null(rownames(performances))) {
    rownames(rel) <- rownames(performances)
    colnames(rel) <- rownames(performances)
  }
  
  class(rel) <- "binary.relation"
  return(rel)
}

matrixOfRelationsToList <- function(rel) {
  if (is.null(rel)) {
    return(NULL)
  }
  relations = matrix(nrow=0, ncol=2)
  for (i in 1:nrow(rel)) {
    for(j in 1:nrow(rel)) {
      if ((rel[i,j] == TRUE) && (i != j)) {
        variant.i <- rownames(rel)[[i]]
        variant.j <- rownames(rel)[[j]]
        relations <- rbind(relations, c(variant.i,variant.j))
      }
    }
  }
  return(relations)
}


checkRelation <- function(perf, base.model, number.of.real.variables, a, b, necessary = FALSE, filter = NULL) {
  ## check vars
  stopifnot(is.logical(necessary))
  if (a == b) {
    return(TRUE)
  }
  alt.vars <- buildAltVariableMatrix(perf)
  if (!is.null(filter)){
    alt.vars <- getFilteredAltVariableMatrix(altVar=alt.vars, filter=filter)
  }
  add.const <- c()
  if (necessary == TRUE) {
    add.const <- buildStrongPreferenceConstraint(b, a, alt.vars)
  } else { ## possible
    add.const <- buildWeakPreferenceConstraint(a, b, alt.vars)
  }
  add.const$lhs <- getNormalizedMatrix(add.const$lhs, width=ncol(base.model$lhs)) 
  all.const <- combineConstraints(base.model, add.const)
  
  ret <- solveModel(all.const, number.of.real.variables)
  #  cat("a", a, "b", b, "code", ret$status$code, "objval", ret$objval, "\n")
  
  if (necessary == TRUE) {
    return(ret$status$code != 0 || ret$objval < MINEPS)
  } else { # possible
    return(ret$status$code == 0 && ret$objval >= MINEPS)
  }
}



