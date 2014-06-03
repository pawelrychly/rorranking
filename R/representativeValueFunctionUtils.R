
getFunctionsFromSolution <- function(perf, nums.of.characteristic.points = NULL, solution=NULL) {
  col.ids <- dimnames(perf)[[2]]
  if (is.null(col.ids)) {
    col.ids <- seq(ncol(perf))
  }
  if (is.null(solution) || (solution$status$code != 0)) {
    return(list())
  } 
  functions <- list() 
  list.of.characteristic.points <- getCharacteristicPoints(perf=perf, nums.of.characteristic.points=nums.of.characteristic.points)
  levels <- getLevels(perf=perf)
  num.of.vars <- getNrVars(levels=levels)
  offsets.for.levels <- getOffsets(levels=levels)
  offsets.for.points <- getOffsets(levels=list.of.characteristic.points)  
  for (i in seq(length(list.of.characteristic.points))) {
    if (length(list.of.characteristic.points[[i]]) == 0) {
      functions[[i]] <- matrix(ncol=2, nrow=length(levels[[i]]), data=0)
      for (j in seq(length(levels[[i]]))) {
        functions[[i]][j, 1] = levels[[i]][[j]]
        functions[[i]][j, 2] = solution$solution[[offsets.for.levels[[i]]+j-1]]
      }
    } else {
      functions[[i]] <- matrix(ncol=2, nrow=length(list.of.characteristic.points[[i]]), data=0)
      for (j in seq(length(list.of.characteristic.points[[i]]))) {
        functions[[i]][j, 1] = list.of.characteristic.points[[i]][[j]]
        functions[[i]][j, 2] = solution$solution[[num.of.vars + offsets.for.points[[i]]+j-1]]
      }
    } 
  }  
  return(functions)
}

buildCompromiseConstraint <- function(a, b, c, d,  altVars) {
  lhs <- altVars[a,]
  lhs <- lhs + altVars[d,] - altVars[b,] - altVars[c,]
  return(list(lhs=lhs, dir=">=", rhs=0))
}

solveModelForRepresentativeFunction <- function(model, number.of.real.variables, num.of.suffix.real.variables=0, maximum=TRUE) {
  number.of.all.variables <- ncol(model$lhs)
  types <- rep('C', number.of.all.variables)
  if (number.of.real.variables < (number.of.all.variables - num.of.suffix.real.variables)){
    position.of.first.binary.variable <- number.of.real.variables + 1
    types[position.of.first.binary.variable : (number.of.all.variables - num.of.suffix.real.variables)] <- 'B'
  }  
  objective <- rep(0, number.of.all.variables)
  objective[number.of.all.variables] <- 1  
  obj <- L_objective(objective)
  roiConst <- L_constraint(L = model$lhs, dir =model$dir, rhs=model$rhs)
  lp <- OP(objective=obj, constraints=roiConst, maximum=maximum, types=types)
  res <- ROI_solve(lp, .solver)
 
  if (res$status$code!=0) {
    stop("Model is infeasible")
  }
  return(res)
}

buildVariantsDiffGreaterThanZero <- function(a, b, altVars) {
  lhs <- altVars[a,]
  lhs <- lhs - altVars[b,]
  
  return(list(lhs=lhs, dir=">=", rhs=0))
}

buildVariantsDiffLessThanZero <- function(a, b, altVars) {
  lhs <- altVars[a,]
  lhs <- lhs - altVars[b,]
  
  return(list(lhs=lhs, dir="<=", rhs=0))
}

getListOfPairsWithoutNecRelations <- function(nec.relations.matrix) {
  num.of.cols <- ncol(nec.relations.matrix)
  num.of.rows <- nrow(nec.relations.matrix)
  list.of.pairs <- list()
  for (i in seq(num.of.rows-1)) {
    for (j in seq(i+1, num.of.cols)) {
      if ((nec.relations.matrix[i, j] == FALSE) && (nec.relations.matrix[j, i] == FALSE) && (i != j)) {
        list.of.pairs <- append(list.of.pairs, list(c(i,j)))
      }
    }
  }
  return(list.of.pairs)
}

getListOfPairsWithNecRelations <- function(nec.relations.matrix) {
  num.of.cols <- ncol(nec.relations.matrix)
  num.of.rows <- nrow(nec.relations.matrix)
  list.of.pairs <- list()
  for (i in seq(num.of.rows)) {
    for (j in seq(num.of.cols)) {
      if ((nec.relations.matrix[i, j] == TRUE) && (i != j)) {
        list.of.pairs <- append(list.of.pairs, list(c(i,j)))
      }
    }
  }
  return(list.of.pairs)
}

buildConstraintsDecreasingDifferenceForPairsWithoutRelations <- function(not.nec.relations, alt.vars, width.of.constraints) {
  additional.constraints <- list()
  diff <- list()
  if (length(not.nec.relations) <= 0) {
    return(list())
  }
  for (i in seq(length(not.nec.relations))) {
    pair <- not.nec.relations[[i]]
    diff <- buildVariantsDiffLessThanZero(a=pair[[1]], b=pair[[2]], altVars=alt.vars)
    diff2 <- buildVariantsDiffLessThanZero(a=pair[[2]], b=pair[[1]], altVars=alt.vars)
    diff$lhs <- getNormalizedMatrix(matrix=diff$lhs, width=width.of.constraints)
    diff2$lhs <- getNormalizedMatrix(matrix=diff2$lhs, width=width.of.constraints)
    additional.constraints <- combineConstraints(additional.constraints, diff)
    row.names(additional.constraints$lhs)[nrow(additional.constraints$lhs)] <- c(paste(pair[[1]], "-", pair[[2]],"-u2", sep=""))
    additional.constraints <- combineConstraints(additional.constraints, diff2)
    row.names(additional.constraints$lhs)[nrow(additional.constraints$lhs)] <- c(paste(pair[[2]], "-", pair[[1]],"-u2", sep=""))
  }
  num.of.rows <- nrow(additional.constraints$lhs)
  new.col <- matrix(data=-1, nrow=num.of.rows, ncol=1)
  additional.constraints$lhs <- cbind(additional.constraints$lhs, new.col)
  return(additional.constraints)
}

buildConstraintsIncreasingDifferenceForNecessaryRelations <- function(nec.relations, alt.vars, width.of.constraints) {
  additional.constraints <- list()
  diff <- list()
  for (i in seq(length(nec.relations))) {
    pair <- nec.relations[[i]]
    diff <- buildVariantsDiffGreaterThanZero(a=pair[[1]], b=pair[[2]], altVars=alt.vars)
    diff$lhs <- getNormalizedMatrix(matrix=diff$lhs, width=width.of.constraints)
    additional.constraints <- combineConstraints(additional.constraints, diff)
    row.names(additional.constraints$lhs)[nrow(additional.constraints$lhs)] <- c(paste(pair[[1]], "-", pair[[2]],"-u", sep=""))
  }
  num.of.rows <-nrow(additional.constraints$lhs)
  new.col <- matrix(data=-1, nrow=num.of.rows, ncol=1)
  additional.constraints$lhs <- cbind(additional.constraints$lhs, new.col)
  return(additional.constraints)
}  

findRepresentativeValueFunctionIterative <- function(perf, strict.vf=FALSE, 
                                                  strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                                                  strong.intensities.of.prefs = NULL,
                                                  weak.intensities.of.prefs = NULL,
                                                  indif.intensities.of.prefs = NULL, 
                                                  rank.related.requirements = NULL, 
                                                  nums.of.characteristic.points = NULL,
                                                  nec.relations.matrix=NULL, k=0.0,
                                                  criteria.by.nodes=NULL, nodeid=NULL) {
  if (is.null(nums.of.characteristic.points)) {
    nums.of.characteristic.points = rep(0, ncol(perf))
  }
  if (is.null(nec.relations.matrix)) {
    return(list())
  }
  filter <- NULL
  if ((!is.null(nodeid)) && (!is.null(criteria.by.nodes))) {
    criteria.set <- criteria.by.nodes[[nodeid]]
    filter <- getCriteriaFilter(perf=perf, criteria=criteria.set)
  }
  alt.vars <- buildAltVariableMatrix(perf=perf)
  if (!is.null(filter)){
    alt.vars <- getFilteredAltVariableMatrix(altVar=alt.vars, filter=filter)
  }
  number.of.real.variables <- getNumberOfVariables(perf=perf, numbers.of.characteristic.points=nums.of.characteristic.points)
  base.model <- buildBaseLPModel(perf=perf, strict.vf=strict.vf, strong.prefs = strong.prefs,
                                 weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                 strong.intensities.of.prefs =  strong.intensities.of.prefs , weak.intensities.of.prefs = weak.intensities.of.prefs,
                                 indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                 rank.related.requirements = rank.related.requirements,
                                 nums.of.characteristic.points=nums.of.characteristic.points, 
                                 criteria.by.nodes=criteria.by.nodes) 
  
  nec.relations <- getListOfPairsWithNecRelations(nec.relations.matrix = nec.relations.matrix)
  not.nec.relations <- getListOfPairsWithoutNecRelations(nec.relations.matrix = nec.relations.matrix)
  diff.for.nec.relations <- 0
  solution <- NULL
  if (length(nec.relations) > 0) {
    constraints.for.nec.relations <- buildConstraintsIncreasingDifferenceForNecessaryRelations(nec.relations=nec.relations,
                                                                                               alt.vars=alt.vars, 
                                                                                               width.of.constraints = ncol(base.model$lhs))
    base.model$lhs <- getNormalizedMatrix(matrix=base.model$lhs, width=ncol(constraints.for.nec.relations$lhs))
    base.model <- combineConstraints(base.model, constraints.for.nec.relations)
    
    solution <- solveModelForRepresentativeFunction(model = base.model, number.of.real.variables = number.of.real.variables, num.of.suffix.real.variables=1, maximum=TRUE)
    if (solution$status$code == 0) {
      diff.for.nec.relations <- solution$objval
      
    }
  }
  if (length(not.nec.relations) > 0) {
    constraints.for.not.nec.relations <- buildConstraintsDecreasingDifferenceForPairsWithoutRelations(not.nec.relations=not.nec.relations,
                                                                                                    alt.vars=alt.vars, 
                                                                                                    width.of.constraints = ncol(base.model$lhs))
    base.model$lhs <- getNormalizedMatrix(matrix=base.model$lhs, width=ncol(constraints.for.not.nec.relations$lhs))
    base.model <- combineConstraints(base.model, constraints.for.not.nec.relations)
  }
  if (length(not.nec.relations) > 0) {
    if (length(nec.relations) > 0) {
      diff.for.nec.relations.constraint = list()
      diff.for.nec.relations.constraint[["lhs"]] <- matrix(data=0, nrow=1, ncol=ncol(base.model$lhs))
      diff.for.nec.relations.constraint$lhs[1, ncol(diff.for.nec.relations.constraint$lhs) - 1] <- 1
      diff.for.nec.relations.constraint$dir <- c(">=")
      diff.for.nec.relations.constraint$rhs <- c((1-k) * diff.for.nec.relations) 
      base.model <- combineConstraints(base.model, diff.for.nec.relations.constraint)
    }
    solution <- solveModelForRepresentativeFunction(model = base.model,
                                                    number.of.real.variables = number.of.real.variables,
                                                    num.of.suffix.real.variables=2,
                                                    maximum=FALSE)  
    
  }
  functions <- getFunctionsFromSolution(perf=perf, nums.of.characteristic.points=nums.of.characteristic.points, solution=solution)  
  return(functions)
}

buildConstraintsForCompromise <- function(nec.relations, not.nec.relations, alt.vars, width.of.constraints) {
  additional.constraints <- list()
  if ((length(nec.relations) > 0) && (length(not.nec.relations) > 0)) {
    for (i in seq(length(nec.relations))) {
      for (j in seq(length(not.nec.relations))) {
        diff <- list()
        a <- nec.relations[[i]][[1]]
        b <- nec.relations[[i]][[2]]
        c <- not.nec.relations[[j]][[1]]
        d <- not.nec.relations[[j]][[2]]
        diff <- buildCompromiseConstraint(a=a, b=b, c=c, d=d, altVars=alt.vars)
        diff$lhs <- getNormalizedMatrix(matrix=diff$lhs, width=width.of.constraints)
        additional.constraints <- combineConstraints(additional.constraints, diff)
        row.names(additional.constraints$lhs)[nrow(additional.constraints$lhs)] <- c(paste(a, "-", b,">=", c, "-",d, "+v", sep=""))
      }  
    }
  }
  num.of.rows <-nrow(additional.constraints$lhs)
  new.col <- matrix(data=-1, nrow=num.of.rows, ncol=1)
  additional.constraints$lhs <- cbind(additional.constraints$lhs, new.col)
  return(additional.constraints)
}  

findRepresentativeValueFunctionCompromise <- function(perf, strict.vf=FALSE, 
                                                     strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                                                     strong.intensities.of.prefs = NULL,
                                                     weak.intensities.of.prefs = NULL,
                                                     indif.intensities.of.prefs = NULL, 
                                                     rank.related.requirements = NULL, 
                                                     nums.of.characteristic.points = NULL,
                                                     nec.relations.matrix=NULL, 
                                                     criteria.by.nodes=NULL, nodeid=NULL) {
  if (is.null(nums.of.characteristic.points)) {
    nums.of.characteristic.points = rep(0, ncol(perf))
  }
  if (is.null(nec.relations.matrix)) {
    return(list())
  }
  filter <- NULL
  if ((!is.null(nodeid)) && (!is.null(criteria.by.nodes))) {
    criteria.set <- criteria.by.nodes[[nodeid]]
    filter <- getCriteriaFilter(perf=perf, criteria=criteria.set)
  }
  alt.vars <- buildAltVariableMatrix(perf=perf)
  if (!is.null(filter)){
    alt.vars <- getFilteredAltVariableMatrix(altVar=alt.vars, filter=filter)
  }
  number.of.real.variables <- getNumberOfVariables(perf=perf, numbers.of.characteristic.points=nums.of.characteristic.points)
  base.model <- buildBaseLPModel(perf=perf, strict.vf=strict.vf, strong.prefs = strong.prefs,
                                 weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                 strong.intensities.of.prefs =  strong.intensities.of.prefs , weak.intensities.of.prefs = weak.intensities.of.prefs,
                                 indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                 rank.related.requirements = rank.related.requirements,
                                 nums.of.characteristic.points=nums.of.characteristic.points, 
                                 criteria.by.nodes=criteria.by.nodes) 
  
  nec.relations <- getListOfPairsWithNecRelations(nec.relations.matrix = nec.relations.matrix)
  not.nec.relations <- getListOfPairsWithoutNecRelations(nec.relations.matrix = nec.relations.matrix)
  constraints <- list()
  if ((length(nec.relations) > 0) && (length(not.nec.relations) > 0)) {
    constraints <-buildConstraintsForCompromise(nec.relations=nec.relations, not.nec.relations=not.nec.relations, alt.vars=alt.vars, width.of.constraints=ncol(base.model$lhs))
  } else if (length(nec.relations) > 0) {
    constraints <- buildConstraintsIncreasingDifferenceForNecessaryRelations(nec.relations=nec.relations,
                                                                             alt.vars=alt.vars, 
                                                                             width.of.constraints = ncol(base.model$lhs))  
  } else if (length(not.nec.relations) > 0) {
    constraints.for.not.nec.relations <- buildConstraintsDecreasingDifferenceForPairsWithoutRelations(not.nec.relations=not.nec.relations,
                                                                                                      alt.vars=alt.vars, 
                                                                                                      width.of.constraints = ncol(base.model$lhs))
  }
  if (ncol(constraints$lhs)>ncol(base.model$lhs)) {
    base.model$lhs <- getNormalizedMatrix(matrix=base.model$lhs, width=ncol(constraints$lhs))
  }
  base.model <- combineConstraints(base.model, constraints)
  solution <- solveModelForRepresentativeFunction(model = base.model,
                                                  number.of.real.variables = number.of.real.variables,
                                                  num.of.suffix.real.variables=1,
                                                  maximum=TRUE)  
    
  
  functions <- getFunctionsFromSolution(perf=perf, nums.of.characteristic.points=nums.of.characteristic.points, solution=solution)  
  return(functions)  
}