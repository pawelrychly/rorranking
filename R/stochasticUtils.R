checkRankRelatedConstraintsForSample <- function(perf, sample, rank.related.constraints=NULL) {
  utility.matrix <- getUtilityMatrix(perf=perf, solution=sample)
  u.values <- countUtilities(utility.matrix)
  ranks <- getRanksMatrix(u.values)
  for (i in nrow(rank.related.constraints)) { 
    constraint.id <- rank.related.constraints[i,1]
    if ((ranks[constraint.id,1] > rank.related.constraints[i,2]) || (ranks[constraint.id,1] < rank.related.constraints[i,3])) {
      return(FALSE)
    }
  }
  return(TRUE)
}

findRankAcceptabilityIndicesHelper <- function(perf, 
                                        strict.vf, 
                                        strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                                        strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                        rank.related.requirements = NULL,
                                        nums.of.characteristic.points=NULL, num.of.samples=100, criteria.by.nodes=NULL, nodeid=NULL) {
  samples <- generateSamples(perf = perf, strict.vf = strict.vf, strong.prefs = strong.prefs,
                             weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                             strong.intensities.of.prefs = strong.intensities.of.prefs,
                             weak.intensities.of.prefs = weak.intensities.of.prefs,
                             indif.intensities.of.prefs = indif.intensities.of.prefs, 
                             rank.related.requirements = rank.related.requirements,
                             nums.of.characteristic.points=nums.of.characteristic.points,
                             num.of.samples=num.of.samples, criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
  
  filter <- NULL
  if ((!is.null(nodeid)) && (!is.null(criteria.by.nodes))) {
    criteria.set <- criteria.by.nodes[[nodeid]]
    for (criterion.id in criteria.set) {
      filter <- c(filter, criterion.id)
    }
  }
  
  
  rank.indices <- matrix(nrow=nrow(perf), ncol=nrow(perf), data=0) 
  row.names(rank.indices) <- row.names(perf)
  num.of.samples <- nrow(samples)
  for (i in seq(num.of.samples)) {
    sample <- samples[i,]
    utility.matrix <- getUtilityMatrix(perf=perf, solution=sample, filter=filter)
    u.values <- countUtilities(utility.matrix)
    ranks <- getRanksMatrix(u.values)
    for (j in seq(nrow(perf))) {
      rank.position <- ranks[j, 1]
      rank.indices[j, rank.position] <- rank.indices[j, rank.position] + 1
    }
  }
  
  for (i in seq(nrow(rank.indices))) {
    for (j in seq(ncol(rank.indices))) {
      rank.indices[i, j] <- (rank.indices[i, j] / num.of.samples) * 100.0
    }
  }
  return(rank.indices)
}

findRelationsAcceptabilityIndicesHelper <- function(perf, 
                                         strict.vf, 
                                         strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                                         strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                         rank.related.requirements = NULL,
                                         nums.of.characteristic.points=NULL, num.of.samples=100, criteria.by.nodes=NULL, nodeid=NULL) {
  samples <- generateSamples(perf = perf, strict.vf = strict.vf, strong.prefs = strong.prefs,
                             weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                             strong.intensities.of.prefs = strong.intensities.of.prefs,
                             weak.intensities.of.prefs = weak.intensities.of.prefs,
                             indif.intensities.of.prefs = indif.intensities.of.prefs, 
                             rank.related.requirements = rank.related.requirements,
                             nums.of.characteristic.points=nums.of.characteristic.points,
                             num.of.samples=num.of.samples, criteria.by.nodes=criteria.by.nodes, nodeid=nodeid)
    
  filter <- NULL
  if ((!is.null(nodeid)) && (!is.null(criteria.by.nodes))) {
    criteria.set <- criteria.by.nodes[[nodeid]]
    for (criterion.id in criteria.set) {
      filter <- c(filter, criterion.id)
    }
  }
  
  relations.indices <- matrix(nrow=nrow(perf), ncol=nrow(perf), data=0) 
  dimnames(relations.indices) <- list(row.names(perf), row.names(perf))
  num.of.samples <- nrow(samples)
  for (i in seq(num.of.samples)) {
    sample <- samples[i,]
    utility.matrix <- getUtilityMatrix(perf=perf, solution=sample, filter=filter)
    u.values <- countUtilities(utility.matrix)
    for (a in row.names(perf)) {
      for (b in row.names(perf)) {
        if (u.values[a,1] >= u.values[b,1]) {
          relations.indices[a,b] <- relations.indices[a,b] + 1
        } 
      }
    }
  }
  for (i in seq(nrow(relations.indices))) {
    for (j in seq(ncol(relations.indices))) {
      relations.indices[i, j] <- (relations.indices[i, j] / num.of.samples) * 100.0
    }
  }
  return(relations.indices)
}

prepareConstraintsForCharacteristicPoints <- function(base.model) {
  additional = list()
  constraint.names <- row.names(base.model$lhs)
  ids <- which(constraint.names == "piece.lin.values")
  if (length(ids) > 0) {
    additional$lhs <- base.model$lhs[ids, ]
    base.model$dir[ids] <- ">="
    additional$dir <- rep("<=", nrow(additional$lhs))
    additional$rhs <- rep(MINEPS * 0.1, nrow(additional$lhs))
    base.model <- combineConstraints(base.model, additional)
  }
  return(base.model)
}


generateSamples <- function(perf, 
                             strict.vf, 
                             strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                             strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                             rank.related.requirements = NULL,
                             nums.of.characteristic.points=NULL, num.of.samples=100, criteria.by.nodes=NULL, nodeid=NULL) {
  
  if (is.null(nums.of.characteristic.points)) {
    nums.of.characteristic.points = rep(0, ncol(perf))
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
  #without rank related requirements
  base.model <- buildBaseLPModel(perf=perf, strict.vf=strict.vf, strong.prefs = strong.prefs,
                                 weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                 strong.intensities.of.prefs =  strong.intensities.of.prefs , weak.intensities.of.prefs = weak.intensities.of.prefs,
                                 indif.intensities.of.prefs = indif.intensities.of.prefs,
                                 rank.related.requirements = rank.related.requirements,
                                 nums.of.characteristic.points=nums.of.characteristic.points, 
                                 criteria.by.nodes=criteria.by.nodes) 
  samples <- NULL
  if (checkConstraintsConsistency(model=base.model, number.of.real.variables=number.of.real.variables)) {
    if (!is.null(rank.related.requirements)) {
      base.model <- buildBaseLPModel(perf=perf, strict.vf=strict.vf, strong.prefs = strong.prefs,
                                     weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                     strong.intensities.of.prefs =  strong.intensities.of.prefs , weak.intensities.of.prefs = weak.intensities.of.prefs,
                                     indif.intensities.of.prefs = indif.intensities.of.prefs,
                                     nums.of.characteristic.points=nums.of.characteristic.points, 
                                     criteria.by.nodes=criteria.by.nodes) 
      base.model <- prepareConstraintsForCharacteristicPoints(base.model)
      hit.and.run.model <- convertToHitandrunProblem(base.model)
      state <- har.init(hit.and.run.model, thin.fn = function(n) { ceiling(log(n + 1)/4 * n^3) },
                        thin = NULL, x0.randomize = FALSE, x0.method = "slacklp", x0 = NULL)
      correct.samples <- 0
      samples <- NULL
      while (correct.samples < num.of.samples) {
        samples.obj <- har.run(state, n.samples = num.of.samples)  
        samples.temp <- samples.obj$samples
        i <- 1
        while((i < nrow(samples.temp)) && (correct.samples < num.of.samples)) {
          if (checkRankRelatedConstraintsForSample(perf=perf, sample=samples.temp[i,], rank.related.constraints=rank.related.requirements)) {
            correct.samples <- correct.samples + 1  
            samples <- rbind(samples, samples.temp[i,])
          } 
          i <- i + 1
        }  
      } 
    } else {
      base.model <- prepareConstraintsForCharacteristicPoints(base.model)
      hit.and.run.model <- convertToHitandrunProblem(base.model)
      state <- har.init(hit.and.run.model, thin.fn = function(n) { ceiling(log(n + 1)/4 * n^3) },
                        thin = NULL, x0.randomize = FALSE, x0.method = "slacklp", x0 = NULL)
      samples.obj <- har.run(state, n.samples = num.of.samples)  
      samples <- samples.obj$samples
    }
  } else {
    stop("model is infeasible")
  }
  
  return(samples)
}

getUtilityMatrix <- function(perf, solution, filter=NULL) {
  utility.matrix = matrix(data=0, ncol=ncol(perf), nrow=nrow(perf), dimnames=dimnames(perf))
  j = 1;
  k = 1;
  levels <- getLevels(perf)
  mapping.all <- list()
  for (values in levels) {
    labels = c()
    v = c()
    map <- list()
    for (value in values) {
      labels <- c(labels, value)
      v <- c(v, solution[j])
      map[[toString(value)]] <- solution[j]
      j = j+1
    }
    mapping.all[[toString(k)]] <- map
    k = k + 1
  }  
  for(variant_index in seq(nrow(perf))) {
    for(value_index in seq(ncol(perf))) {
      v <- perf[variant_index, value_index]
      utility.matrix[variant_index, value_index] <- mapping.all[[toString(value_index)]][[toString(v)]]
    }  
  }
  if (!is.null(filter)) {
    utility.matrix <- subset(x = utility.matrix, select=filter)
  }
  return(utility.matrix)
}

countUtilities <- function(utility.matrix) {
  
  result <- matrix(ncol=1, nrow=nrow(utility.matrix))
  row.names(result) <- row.names(utility.matrix)
  for (i in seq(nrow(utility.matrix))) {
      result[i,1] <- sum(utility.matrix[i,])
  }
  return(result)
}

getRanksMatrix <- function(utilities.matrix) {
  result <- matrix(ncol=1, nrow=nrow(utilities.matrix))
  row.names(result) <- row.names(utilities.matrix)
  
  for (i in seq(nrow(utilities.matrix))) {
   
    result[i,1] <- length(which(utilities.matrix >= (utilities.matrix[i,1] + MINEPS))) + 1
  }
  return(result)
}


convertToHitandrunProblem <- function(model) {
  model$dir[which(model$dir == "==")] <- "="
  greater.of.equal <- which(model$dir == ">=")
  for (index in greater.of.equal) {
    model$rhs[index] <- -1 * model$rhs[index]
    model$lhs[index, ] <- -1 * model$lhs[index, ]
  }
  model$dir[greater.of.equal] <- "<="
  names(model)[1] <- "constr"
  return(model)  
}


