solveModelForRankRelatedUmissingProblem <- function(performances, model, number.of.real.variables, maximum) {
  number.of.umissing.related.binary.variables <- nrow(performances)
  number.of.all.variables <- ncol(model$lhs)
  types <- rep('C', number.of.all.variables)
  position.of.first.binary.variable <- number.of.real.variables + 1
  types[position.of.first.binary.variable : number.of.all.variables] <- 'B' 
  position.of.u <- number.of.all.variables - number.of.umissing.related.binary.variables
  types[position.of.u] <- 'C'
  objective <- rep(0, ncol(model$lhs))
  objective[position.of.u] <- 1
  objective <- getNormalizedMatrix(matrix=objective, width=ncol(model$lhs))
  obj <- L_objective(objective)
  roiConst <- L_constraint(L = model$lhs, dir =model$dir, rhs=model$rhs)
  lp <- OP(objective=obj, constraints=roiConst, maximum=maximum, types=types)
  res <- ROI_solve(lp, .solver)
  return(res)
}

findQLimesRanks <- function(perf, a, k,  start.q, is.possible=FALSE, strict.vf,
                            strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                            strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                            rank.related.requirements = NULL,
                            nums.of.characteristic.points=NULL, which.attributes = NULL,
                            direction.up = 1, precision=0.005, max=FALSE, criteria.by.nodes=NULL, nodeid=NULL){
  q <- start.q
  old.perf = perf
  new.perf = perf
  diff <- q / 2
  q <- q + (direction.up * diff)
  #diff <- diff /2
  new.perf <- pfaImproveVariant(perf, a, q, which.attributes = which.attributes)
  old.perf <- new.perf
  repeat {    
   
    if (diff < precision) {
      constraints = list()
      if (max == TRUE) {
        constraints <- pfaRanksBuildConstraintsForMax(perf = new.perf, a =a, k =k, strict.vf = strict.vf,
                                                      strong.prefs=strong.prefs,weak.prefs=weak.prefs, indif.prefs = indif.prefs,
                                                      strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                      rank.related.requirements = rank.related.requirements,
                                                      nums.of.characteristic.points=nums.of.characteristic.points, 
                                                      criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
      } else {
        constraints <- pfaRanksBuildConstraintsForMin(perf = new.perf, a =a, k =k, strict.vf = strict.vf,
                                                      strong.prefs=strong.prefs,weak.prefs=weak.prefs, indif.prefs = indif.prefs,
                                                      strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                      rank.related.requirements = rank.related.requirements,
                                                      nums.of.characteristic.points=nums.of.characteristic.points, 
                                                      criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
      }
      solution = getSolution(new.perf, constraints, nums.of.characteristic.points = nums.of.characteristic.points)
      result <- list()
      if (length(solution) > 0) {
        result <- solution
      }
      result[['result']] <- q
      return(result)
    }
    if (equalMatrix(new.perf, old.perf)) {
      new.q <- q + (direction.up * diff/2)
      diff <- diff /2
    } else {
      new.q <- q - (direction.up * diff/2)
      diff <- diff /2
    }
    q <- new.q
    old.perf <- new.perf
    new.perf <- pfaImproveVariant(perf, a, q, which.attributes = which.attributes)
  }
} 


binarySearchForNecessaryRanks <- function(perf, a, k, strict.vf, start.q, 
                                          strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                          strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                          rank.related.requirements = NULL,
                                          nums.of.characteristic.points=NULL, which.attributes = NULL, 
                                          precision=0.005, criteria.by.nodes=NULL, nodeid=NULL) {
 
  q <- start.q
  diff <- q/2
  new.perf <- perf
  
  repeat {
    
    old.perf <- new.perf
    new.perf <- pfaImproveVariant(perf, a, q, which.attributes)
    number.of.real.variables <- getNumberOfVariables(perf=new.perf, numbers.of.characteristic.points=nums.of.characteristic.points)
    constraints <- pfaRanksBuildConstraintsForMax(perf = new.perf, a =a, k =k, strict.vf = strict.vf,
                                                  strong.prefs=strong.prefs,weak.prefs=weak.prefs, indif.prefs = indif.prefs,
                                                  strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                  rank.related.requirements = rank.related.requirements,
                                                  nums.of.characteristic.points=nums.of.characteristic.points, 
                                                  criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
    if (diff < precision) {
      new.perf <- pfaImproveVariant(perf, a, q + precision, which.attributes)
      constraints <- pfaRanksBuildConstraintsForMax(perf = new.perf, a =a, k =k, strict.vf = strict.vf,
                                                    strong.prefs=strong.prefs,weak.prefs=weak.prefs, indif.prefs = indif.prefs,
                                                    strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                    rank.related.requirements = rank.related.requirements,
                                                    nums.of.characteristic.points=nums.of.characteristic.points, 
                                                    criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
      solution = getSolution(new.perf, constraints, nums.of.characteristic.points = nums.of.characteristic.points)
      result <- list()
      if (length(solution) > 0) {
        result <- solution
      }
      result[['result']] <- q + precision
      return(result)
    }
    if (!checkConstraintsConsistency(model = constraints, number.of.real.variables=number.of.real.variables)){       
      new.q <- q - (diff / 2)
      diff <- q - new.q
    } else { #feasible - jeśli osiągalne, to musimy zwiększyć wartość Necessary
      new.q <- q + (diff / 2)
      diff <- new.q - q
    }
    q <- new.q
  }
}

binarySearchForPossibleRanks <- function(perf, a, k, strict.vf, start.q, 
                                         strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                         strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                         rank.related.requirements = NULL,
                                         nums.of.characteristic.points=NULL, which.attributes = NULL, 
                                         precision=0.005, criteria.by.nodes=NULL, nodeid=NULL) {
  q <- start.q
  diff <- q/2
  new.perf <- perf
  repeat {
    old.perf <- new.perf
    new.perf <- pfaImproveVariant(perf, a, q, which.attributes)
    number.of.real.variables <- getNumberOfVariables(perf=new.perf, numbers.of.characteristic.points=nums.of.characteristic.points)
    
    constraints <- pfaRanksBuildConstraintsForMin(perf = new.perf, a =a, k =k, strict.vf = strict.vf,
                                                  strong.prefs=strong.prefs,weak.prefs=weak.prefs, indif.prefs = indif.prefs,
                                                  strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                  rank.related.requirements = rank.related.requirements,
                                                  nums.of.characteristic.points=nums.of.characteristic.points, 
                                                  criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
    if (diff < precision) {
      new.perf <- pfaImproveVariant(perf, a, q + precision, which.attributes)
      constraints <-  pfaRanksBuildConstraintsForMin(perf = new.perf, a =a, k =k, strict.vf = strict.vf,
                                                     strong.prefs=strong.prefs,weak.prefs=weak.prefs, indif.prefs = indif.prefs,
                                                     strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                     rank.related.requirements = rank.related.requirements,
                                                     nums.of.characteristic.points=nums.of.characteristic.points, 
                                                     criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
      solution = getSolution(new.perf, constraints, nums.of.characteristic.points = nums.of.characteristic.points)
      result <- list()
      if (length(solution) > 0) {
        result <- solution
      }
      result[['result']] <- q + precision
      return(result)
    }
    if (checkConstraintsConsistency(model=constraints, number.of.real.variables=number.of.real.variables)){       
      new.q <- q - (diff / 2)
      diff <- q - new.q
    } else { #infeasible - jeśli nieosiągalne, to musimy zwiększyć wartość 
      new.q <- q + (diff / 2)
      diff <- new.q - q
    }
    q <- new.q
  }
}


pfaRanksBuildConstraintsForMin<- function(perf, a, k, strict.vf = TRUE,
                                          strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                          strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                          rank.related.requirements = NULL,
                                          nums.of.characteristic.points=c(), criteria.by.nodes=NULL, nodeid=NULL) {
  #a - index of variant a
  #b - index of variant b
  alt.vars <- buildAltVariableMatrix(perf)
  if ((!is.null(nodeid)) && (!is.null(criteria.by.nodes))) {
    criteria.set <- criteria.by.nodes[[nodeid]]
    filter <- getCriteriaFilter(perf=perf, criteria=criteria.set)
    alt.vars <- getFilteredAltVariableMatrix(altVar=alt.vars, filter=filter)
  }
  
  base.model <- buildBaseLPModel(perf=perf, strict.vf=strict.vf, strong.prefs = strong.prefs,
                                 weak.prefs = weak.prefs, indif.prefs = indif.prefs, 
                                 strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                 rank.related.requirements = rank.related.requirements,
                                 nums.of.characteristic.points=nums.of.characteristic.points, criteria.by.nodes=criteria.by.nodes)
  
  pref.constraints <- list()
  number.of.variables <- nrow(perf) 
  all.binaries <- vector()
  
  binaries.sum.constraint <- list()
  binaries.sum.constraint$lhs <- matrix(ncol= (number.of.variables), nrow =  1, data=0)
  binaries.sum.constraint$dir <- "<="
  binaries.sum.constraint$rhs <- k - 1
  
  for (b in seq(number.of.variables)) {
    if (b != a) {
      a.pref.b <- list()
      binaries <- matrix(ncol= (number.of.variables), nrow =  1, data=0)
      binaries[b] <- M_BIG_NUMBER
      all.binaries <- rbind(all.binaries, binaries)
      binaries.sum.constraint$lhs[b] <- 1
      # Weak
      a.pref.b <- buildWeakPreferenceConstraint(a, b, alt.vars) 
      pref.constraints <- combineConstraints(pref.constraints, a.pref.b)
    }
  }
  
  if (ncol(base.model$lhs) >= ncol(pref.constraints$lhs)) {
    pref.constraints$lhs <- getNormalizedMatrix(pref.constraints$lhs, ncol(base.model$lhs))  
  } else {
    base.model$lhs <- getNormalizedMatrix(base.model$lhs, ncol(pref.constraints$lhs))
  }
  pref.constraints$lhs <- cbind(pref.constraints$lhs, all.binaries)
  pref.constraints$rhs[1:length(pref.constraints$rhs)] <- 0
  
  base.model$lhs <- getNormalizedMatrix(base.model$lhs, ncol(pref.constraints$lhs))
  binaries.sum.constraint$lhs <- getNormalizedMatrix(binaries.sum.constraint$lhs, ncol(pref.constraints$lhs), right=FALSE)
  all.constraints <- combineConstraints(base.model, pref.constraints, binaries.sum.constraint)
  return(all.constraints)
}


pfaRanksBuildConstraintsForMax <- function(perf, a, k, strict.vf = TRUE,
                                           strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                           strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                           rank.related.requirements = NULL,
                                           nums.of.characteristic.points=c(), criteria.by.nodes=NULL, nodeid=NULL) {
  #a - index of variant a
  #b - index of variant b
  
  alt.vars <- buildAltVariableMatrix(perf)
  if ((!is.null(nodeid)) && (!is.null(criteria.by.nodes))) {
    criteria.set <- criteria.by.nodes[[nodeid]]
    filter <- getCriteriaFilter(perf=perf, criteria=criteria.set)
    alt.vars <- getFilteredAltVariableMatrix(altVar=alt.vars, filter=filter)
  }
  
  base.model <- buildBaseLPModel(perf=perf, strict.vf=strict.vf, strong.prefs = strong.prefs,
                                 weak.prefs = weak.prefs, indif.prefs = indif.prefs, 
                                 strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                 rank.related.requirements = rank.related.requirements,
                                 nums.of.characteristic.points=nums.of.characteristic.points, criteria.by.nodes=criteria.by.nodes)
  
  pref.constraints <- list()
  number.of.variables <- nrow(perf) 
  all.binaries <- vector()
  
  binaries.sum.constraint <- list()
  binaries.sum.constraint$lhs = matrix(ncol= (number.of.variables), nrow =  1, data=0)
  binaries.sum.constraint$dir = ">="
  binaries.sum.constraint$rhs = k
  
  for (b in seq(number.of.variables)) {
    if (b != a) {
      a.pref.b <- list()
      binaries <- matrix(ncol= (number.of.variables), nrow =  1, data=0)
      binaries[b] <- M_BIG_NUMBER
      all.binaries <- rbind(all.binaries, binaries)
      binaries.sum.constraint$lhs[b] <- 1
      a.pref.b <- buildWeakPreferenceConstraint(a, b, alt.vars) 
      a.pref.b$lhs[length(a.pref.b$lhs)] <- 1
      a.pref.b$dir <- "<="
      pref.constraints <- combineConstraints(pref.constraints, a.pref.b)
    }
  }
  
  if (ncol(base.model$lhs) >= ncol(pref.constraints$lhs)) {
    pref.constraints$lhs <- getNormalizedMatrix(pref.constraints$lhs, ncol(base.model$lhs))  
  } else {
    base.model$lhs <- getNormalizedMatrix(base.model$lhs, ncol(pref.constraints$lhs))
  }
  pref.constraints$lhs <- cbind(pref.constraints$lhs, all.binaries)
  pref.constraints$rhs[1:length(pref.constraints$rhs)] <- M_BIG_NUMBER
  base.model$lhs <- getNormalizedMatrix(base.model$lhs, ncol(pref.constraints$lhs))
  binaries.sum.constraint$lhs <- getNormalizedMatrix(binaries.sum.constraint$lhs, ncol(pref.constraints$lhs), right=FALSE)
  all.constraints <-combineConstraints(base.model, pref.constraints, binaries.sum.constraint)
  return(all.constraints)
}


pfaFindQPossibleRankDeterioration <- function(perf, a, k, strict.vf,
                                              strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                              strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                              rank.related.requirements = NULL,
                                              nums.of.characteristic.points=c(), which.attributes = NULL, precision=0.005, criteria.by.nodes=NULL, nodeid=NULL) {
  new.perf = perf
  q <- 1
  stop.searching = FALSE
  repeat { 
    old.perf = new.perf
    new.perf <- pfaImproveVariant(perf, a, q, which.attributes = which.attributes)
    number.of.real.variables <- getNumberOfVariables(perf=new.perf, numbers.of.characteristic.points=nums.of.characteristic.points)
    
    if (equalMatrix(new.perf, old.perf) && q < 1) {
      stop.searching = TRUE  
    }
    constraints <- pfaRanksBuildConstraintsForMin(perf = new.perf, a =a, k =k, strict.vf = strict.vf,
                                                  strong.prefs=strong.prefs,weak.prefs=weak.prefs, indif.prefs = indif.prefs,
                                                  strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                  rank.related.requirements = rank.related.requirements,
                                                  nums.of.characteristic.points=nums.of.characteristic.points, criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
    consistent <- checkConstraintsConsistency(model = constraints, number.of.real.variables = number.of.real.variables)
    if (!consistent) {
      if (q == 1) {
        return(list("status"="Model is infeasible"))
      }
      
      result <- binarySearchForPossibleRanks(perf = perf, a = a, k = k, strict.vf = strict.vf, start.q = 2*q,
                                             strong.prefs = strong.prefs, weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                             strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                             rank.related.requirements = rank.related.requirements,
                                             nums.of.characteristic.points=nums.of.characteristic.points,
                                             which.attributes = which.attributes, precision=precision, criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)      
      return(result);
    } else {
      if (stop.searching) {    
        return(
          findQLimesRanks( perf, a, k=k,  start.q = 2*q, is.possible=TRUE, strict.vf=strict.vf, 
                           strong.prefs = strong.prefs, weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                           strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                           rank.related.requirements = rank.related.requirements,
                           which.attributes = which.attributes,
                           direction.up = 1, precision=precision, nums.of.characteristic.points=nums.of.characteristic.points,
                           max=FALSE,criteria.by.nodes=criteria.by.nodes, nodeid=nodeid )) ##search limes
      }
    }
    q <- q * 0.5
  }
}

pfaFindQPossibleRankImprovement <- function(perf, a, k, strict.vf,
                                            strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                            strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                            rank.related.requirements = NULL,
                                            nums.of.characteristic.points=c(), which.attributes = NULL, precision=0.005, criteria.by.nodes=NULL, nodeid=NULL) {
  new.perf = perf
  q <- 1
  stop.searching = FALSE
  repeat {   
    old.perf = new.perf
    new.perf <- pfaImproveVariant(perf, a, q, which.attributes = which.attributes)
    number.of.real.variables <- getNumberOfVariables(perf=new.perf, numbers.of.characteristic.points=nums.of.characteristic.points)
    if (equalMatrix(new.perf, old.perf) && q > 1) {
      stop.searching = TRUE  
    }
    constraints <- pfaRanksBuildConstraintsForMin(perf = new.perf, a =a, k =k, strict.vf = strict.vf,
                                                  strong.prefs=strong.prefs,weak.prefs=weak.prefs, indif.prefs = indif.prefs,
                                                  strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                  rank.related.requirements = rank.related.requirements,
                                                  nums.of.characteristic.points=nums.of.characteristic.points, criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
    consistent <- checkConstraintsConsistency(model = constraints, number.of.real.variables = number.of.real.variables)
    if (consistent) {
      if (q == 1) {        
        solution = getSolution(new.perf, constraints, nums.of.characteristic.points = nums.of.characteristic.points)
        result <- list()
        if (length(solution) > 0) {
          result <- solution
        }
        result[['result']] <- 1
        return(result)
      }
      result <- binarySearchForPossibleRanks(perf = perf, a = a, k = k, strict.vf = strict.vf, start.q = q,
                                        strong.prefs = strong.prefs, weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                        strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                        rank.related.requirements = rank.related.requirements,
                                        nums.of.characteristic.points=nums.of.characteristic.points,
                                        which.attributes = which.attributes, precision=precision, criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)      
      return(result);
    } else {
      if (stop.searching) {
        return(list("status"="Model is infeasible"))
      }
    }
    q <- q * 2
  }
}

pfaFindQNecessaryRankImprovement <- function(perf, a, k, strict.vf,
                                             strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                             strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                             rank.related.requirements = NULL,
                                             nums.of.characteristic.points=c(), which.attributes = NULL, precision=0.005, criteria.by.nodes=NULL, nodeid=NULL) {
  
  
  new.perf = perf
  q <- 1
  stop.searching = FALSE
  repeat {    
    old.perf = new.perf
    new.perf <- pfaImproveVariant(perf, a, q, which.attributes = which.attributes)
    if (equalMatrix(new.perf, old.perf) && q > 1) {
      stop.searching = TRUE  
    }
    number.of.real.variables <- getNumberOfVariables(perf=new.perf, numbers.of.characteristic.points=nums.of.characteristic.points)
    constraints <- pfaRanksBuildConstraintsForMax(perf = new.perf, a =a, k =k, strict.vf = strict.vf,
                                                  strong.prefs=strong.prefs,weak.prefs=weak.prefs, indif.prefs = indif.prefs,
                                                  strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                  rank.related.requirements = rank.related.requirements,
                                                  nums.of.characteristic.points=nums.of.characteristic.points, 
                                                  criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
    consistent <- checkConstraintsConsistency(model = constraints, number.of.real.variables = number.of.real.variables)
    
    if (!consistent) {
      if (q == 1) {
        return(list("status"="Model is infeasible"))
      }
      result <- binarySearchForNecessaryRanks(perf = perf, a = a, k = k, strict.vf = strict.vf, start.q = q,
                                         strong.prefs = strong.prefs, weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                         strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                         rank.related.requirements = rank.related.requirements,
                                         nums.of.characteristic.points=nums.of.characteristic.points,
                                         which.attributes = which.attributes, precision=precision, criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
      return(result);
    } else {
      if (stop.searching) {
        return(list("status"="Model is infeasible"))
      }
    }
    q <- q * 2
  }
}

pfaFindQNecessaryRankDeterioration <- function(perf, a, k, strict.vf,
                                               strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                               strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                               rank.related.requirements = NULL,
                                               nums.of.characteristic.points=c(), which.attributes = NULL, precision=0.005, criteria.by.nodes=NULL, nodeid=NULL) {
  new.perf = perf
  q <- 1
  stop.searching = FALSE
  repeat {    
    old.perf = new.perf
    new.perf <- pfaImproveVariant(perf, a, q, which.attributes = which.attributes)
    number.of.real.variables <- getNumberOfVariables(perf=new.perf, numbers.of.characteristic.points=nums.of.characteristic.points)
    if (equalMatrix(new.perf, old.perf) && q < 1) {
      stop.searching = TRUE  
    }
    constraints <- pfaRanksBuildConstraintsForMax(perf = new.perf, a =a, k =k, strict.vf = strict.vf,
                                                  strong.prefs=strong.prefs,weak.prefs=weak.prefs, indif.prefs = indif.prefs,
                                                  strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                  rank.related.requirements = rank.related.requirements,
                                                  nums.of.characteristic.points=nums.of.characteristic.points, 
                                                  criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
    consistent <- checkConstraintsConsistency(model = constraints, number.of.real.variables = number.of.real.variables)
    if (consistent) {
      if (q == 1) {
        solution = getSolution(new.perf, constraints, nums.of.characteristic.points = nums.of.characteristic.points)
        result <- list()
        if (length(solution) > 0) {
          result <- solution
        }
        result[['result']] <- 1
        return(result)
      }
      result <- binarySearchForNecessaryRanks(perf = perf, a = a, k = k, strict.vf = strict.vf, start.q = 2*q,
                                              strong.prefs = strong.prefs, weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                              strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                              rank.related.requirements = rank.related.requirements,
                                              nums.of.characteristic.points=nums.of.characteristic.points,
                                              which.attributes = which.attributes, precision=precision, criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
      return(result);
    } else {
      if (stop.searching) {               
        result <- findQLimesRanks( perf, a, k=k,  start.q = 2*q, is.possible=FALSE, strict.vf=strict.vf, 
                         strong.prefs = strong.prefs, weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                         strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                         rank.related.requirements = rank.related.requirements,
                         which.attributes = which.attributes,
                         direction.up = 1, precision=precision, nums.of.characteristic.points=nums.of.characteristic.points,
                         max=TRUE,criteria.by.nodes=criteria.by.nodes, nodeid=nodeid ) ##search limes
        return(result)
      }
    }
    q <- q * 0.5
  }
}


possibleComprehensiveRankImprovement <- function(perf, a, k, strict.vf, strong.prefs=NULL,
                                                 weak.prefs=NULL, indif.prefs = NULL, 
                                                 strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                 rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                 which.attributes = NULL, greater.than.one = TRUE, criteria.by.nodes=NULL, nodeid=NULL) {
  # greater than one  = TRUE q >= 1 else q < 1  
  if (!is.numeric(a)) {
    criteria <- dimnames(perf)[[1]]
    a <- which(criteria == a)
  }

  if (greater.than.one) {
    q <- pfaFindQPossibleRankImprovement(perf = perf, a = a, k = k, strict.vf = strict.vf,
                                         strong.prefs=strong.prefs,weak.prefs=weak.prefs, indif.prefs = indif.prefs,
                                         strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                         rank.related.requirements = rank.related.requirements,
                                         nums.of.characteristic.points=nums.of.characteristic.points, which.attributes = which.attributes, precision=precision, criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
  } else {
    q <- pfaFindQPossibleRankDeterioration(perf = perf, a = a, k = k, strict.vf = strict.vf,
                                           strong.prefs=strong.prefs,weak.prefs=weak.prefs, indif.prefs = indif.prefs,
                                           strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                           rank.related.requirements = rank.related.requirements,
                                           nums.of.characteristic.points=nums.of.characteristic.points, which.attributes = which.attributes, precision=precision, criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
  }
  return(q)
}


necessaryComprehensiveRankImprovement <- function(perf, a, k, strict.vf, strong.prefs=NULL,
                                                  weak.prefs=NULL, indif.prefs = NULL, 
                                                  strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                  rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                  which.attributes = NULL, greater.than.one = TRUE, criteria.by.nodes=NULL, nodeid=NULL) {
  # greater than one  = TRUE q >= 1 else q < 1  
  if (!is.numeric(a)) {
    criteria <- dimnames(perf)[[1]]
    a <- which(criteria == a)
  }
  if (greater.than.one) {
    q <- pfaFindQNecessaryRankImprovement(perf = perf, a = a, k = k, strict.vf = strict.vf,
                                          strong.prefs=strong.prefs,weak.prefs=weak.prefs, indif.prefs = indif.prefs,
                                          strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                          rank.related.requirements = rank.related.requirements,
                                          nums.of.characteristic.points=nums.of.characteristic.points, which.attributes = which.attributes, precision=precision, criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
  } else {
    q <- pfaFindQNecessaryRankDeterioration(perf = perf, a = a, k = k, strict.vf = strict.vf,
                                            strong.prefs=strong.prefs,weak.prefs=weak.prefs, indif.prefs = indif.prefs,
                                            strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                            rank.related.requirements = rank.related.requirements,
                                            nums.of.characteristic.points=nums.of.characteristic.points, which.attributes = which.attributes, precision=precision, criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
  }
  return(q)
}


pfaRanksBuildConstraintsForPossibly <- function(perf, a, k, strict.vf = TRUE,
                                                strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL, 
                                                strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, improvement = FALSE, criteria.by.nodes=NULL, nodeid=NULL) {
  #a - index of variant a
  #b - index of variant b
  alt.vars <- buildAltVariableMatrix(perf)
  if ((!is.null(nodeid)) && (!is.null(criteria.by.nodes))) {
    criteria.set <- criteria.by.nodes[[nodeid]]
    filter <- getCriteriaFilter(perf=perf, criteria=criteria.set)
    alt.vars <- getFilteredAltVariableMatrix(altVar=alt.vars, filter=filter)
  }
  base.model <- buildBaseLPModel(perf=perf, strict.vf=strict.vf, strong.prefs = strong.prefs,
                                 weak.prefs = weak.prefs, indif.prefs = indif.prefs, 
                                 strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                 rank.related.requirements = rank.related.requirements,
                                 nums.of.characteristic.points=nums.of.characteristic.points, criteria.by.nodes=criteria.by.nodes)
  
  pref.constraints <- list()
  number.of.variables <- nrow(perf) 
  all.binaries <- vector()
  binaries.sum.constraint <- list()
  binaries.sum.constraint$lhs <- matrix(ncol= (number.of.variables), nrow =  1, data=0)
  binaries.sum.constraint$dir <- "<="
  binaries.sum.constraint$rhs <- k - 1
  u <- 1
  if (improvement) {
    u <- 1
  } else {
    u <- -1
  }
  position.of.u = 0
  for (b in seq(number.of.variables)) {
    if (b != a) {
      a.pref.b <- list()
      binaries <- matrix(ncol= (number.of.variables), nrow =  1, data=0)
      binaries[b] <- M_BIG_NUMBER
      all.binaries <- rbind(all.binaries, binaries)
      binaries.sum.constraint$lhs[b] <- 1
      # Weak
      a.pref.b <- buildWeakPreferenceConstraint(a, b, alt.vars) 
      pref.constraints <- combineConstraints(pref.constraints, a.pref.b)
    }
  }
  if (ncol(base.model$lhs) >= ncol(pref.constraints$lhs)) {
    pref.constraints$lhs <- getNormalizedMatrix(pref.constraints$lhs, ncol(base.model$lhs))  
  } else {
    base.model$lhs <- getNormalizedMatrix(base.model$lhs, ncol(pref.constraints$lhs))
  }
  pref.constraints$lhs <- cbind(pref.constraints$lhs, u) 
  position.of.u = ncol(pref.constraints$lhs)
  pref.constraints$lhs <- cbind(pref.constraints$lhs, all.binaries)
  #constraints u >= 0
  u.greater <- list(
    lhs = rep(0, ncol(pref.constraints$lhs)),
    dir = ">=",
    rhs = 0
  )
  u.greater$lhs[position.of.u] = 1
  pref.constraints$rhs[1:length(pref.constraints$rhs)] <- 0
  pref.constraints <- combineConstraints(pref.constraints, u.greater)
  base.model$lhs <- getNormalizedMatrix(base.model$lhs, ncol(pref.constraints$lhs))
  binaries.sum.constraint$lhs <- getNormalizedMatrix(binaries.sum.constraint$lhs, ncol(pref.constraints$lhs), right=FALSE)
  all.constraints <- combineConstraints(base.model, pref.constraints, binaries.sum.constraint)
  return(all.constraints)
}


#based on MAX
pfaRanksBuildConstraintsForNecessary <- function(perf, a, k, strict.vf = TRUE,
                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL, 
                                                 strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                 rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, improvement = FALSE, criteria.by.nodes=NULL, nodeid=NULL) {
  #a - index of variant a
  #b - index of variant b
  alt.vars <- buildAltVariableMatrix(perf)
  if ((!is.null(nodeid)) && (!is.null(criteria.by.nodes))) {
    criteria.set <- criteria.by.nodes[[nodeid]]
    filter <- getCriteriaFilter(perf=perf, criteria=criteria.set)
    alt.vars <- getFilteredAltVariableMatrix(altVar=alt.vars, filter=filter)
  }
  base.model <- buildBaseLPModel(perf=perf, strict.vf=strict.vf, strong.prefs = strong.prefs,
                                 weak.prefs = weak.prefs, indif.prefs = indif.prefs, 
                                 strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                 rank.related.requirements = rank.related.requirements,
                                 nums.of.characteristic.points=nums.of.characteristic.points, criteria.by.nodes=criteria.by.nodes)
  pref.constraints <- list()
  number.of.variables <- nrow(perf) 
  all.binaries <- vector()
  
  binaries.sum.constraint <- list()
  binaries.sum.constraint$lhs = matrix(ncol= (number.of.variables), nrow =  1, data=0)
  binaries.sum.constraint$dir = ">="
  binaries.sum.constraint$rhs = k
  
  u <- 1
  if (improvement) {
    u <- 1
  } else {
    u <- -1
  }
  position.of.u = 0
  
  for (b in seq(number.of.variables)) {
    if (b != a) {
      a.pref.b <- list()
      binaries <- matrix(ncol= (number.of.variables), nrow =  1, data=0)
      binaries[b] <- M_BIG_NUMBER
      all.binaries <- rbind(all.binaries, binaries)
      binaries.sum.constraint$lhs[b] <- 1
      a.pref.b <- buildWeakPreferenceConstraint(a, b, alt.vars) 
      a.pref.b$lhs[length(a.pref.b$lhs)] <- 1
      a.pref.b$dir <- "<="
      pref.constraints <- combineConstraints(pref.constraints, a.pref.b)
    }
  }
  if (ncol(base.model$lhs) >= ncol(pref.constraints$lhs)) {
    pref.constraints$lhs <- getNormalizedMatrix(pref.constraints$lhs, ncol(base.model$lhs))  
  } else {
    base.model$lhs <- getNormalizedMatrix(base.model$lhs, ncol(pref.constraints$lhs))
  }
  pref.constraints$lhs <- cbind(pref.constraints$lhs, u) 
  position.of.u = ncol(pref.constraints$lhs)
  pref.constraints$lhs <- cbind(pref.constraints$lhs, all.binaries)
  pref.constraints$rhs[1:length(pref.constraints$rhs)] <- M_BIG_NUMBER
  #constraints u >= 0
  u.greater <- list(
    lhs = rep(0, ncol(pref.constraints$lhs)),
    dir = ">=",
    rhs = 0
  )
  u.greater$lhs[position.of.u] = 1
  pref.constraints <- combineConstraints(pref.constraints, u.greater)
  base.model$lhs <- getNormalizedMatrix(base.model$lhs, ncol(pref.constraints$lhs))
  binaries.sum.constraint$lhs <- getNormalizedMatrix(binaries.sum.constraint$lhs, ncol(pref.constraints$lhs), right=FALSE)
  all.constraints <-combineConstraints(base.model, pref.constraints, binaries.sum.constraint)
  return(all.constraints)
}

pfaUmissingNecessaryOrPossibleComprehensiveRankImprovement <- function(perf, a, k, strict.vf, is.possibly.preffered,
                                                                       strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                       strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                       rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, improvement=TRUE,  criteria.by.nodes=NULL, nodeid=NULL) {
  if (!is.numeric(a)) {
    criteria <- dimnames(perf)[[1]]
    a <- which(criteria == a)
  }
  constraints <- list()
  if (is.possibly.preffered) {
    constraints <- pfaRanksBuildConstraintsForPossibly(perf = perf, a = a, k = k, strict.vf = strict.vf,
                                                       strong.prefs=strong.prefs,weak.prefs=weak.prefs, indif.prefs = indif.prefs, 
                                                       strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                       rank.related.requirements = rank.related.requirements, nums.of.characteristic.points=nums.of.characteristic.points, improvement = improvement,  criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
  } else {
    constraints <- pfaRanksBuildConstraintsForNecessary(perf = perf, a = a, k = k, strict.vf = strict.vf,
                                                        strong.prefs=strong.prefs,weak.prefs=weak.prefs, indif.prefs = indif.prefs, 
                                                        strong.intensities.of.prefs = strong.intensities.of.prefs, weak.intensities.of.prefs = weak.intensities.of.prefs, indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                                        rank.related.requirements = rank.related.requirements, nums.of.characteristic.points=nums.of.characteristic.points, improvement = improvement,  criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)    
  }
  maximum = TRUE
  if (((is.possibly.preffered) && (improvement)) || ((!is.possibly.preffered) && (!improvement))) { 
    maximum = FALSE
  } else {
    maximum = TRUE
  }
  number.of.real.variables <- getNumberOfVariables(perf=perf, numbers.of.characteristic.points=nums.of.characteristic.points)
  ret <- solveModelForRankRelatedUmissingProblem(performances =perf, model=constraints, number.of.real.variables=number.of.real.variables, maximum=  maximum)
  solution = getSolution(perf=perf, nums.of.characteristic.points = nums.of.characteristic.points, ret=ret)
  result <- list()
  if (length(solution) > 0) {
    result <- solution
  }
  result[['result']] <- ret$objval
  return(result)
}





