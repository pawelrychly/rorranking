
prFindAllPreferentionalReducts <- function(e.ts, e.pi.rank, name, number.of.real.variables = 0) {
  prs <- list()
  
  if (!prCheckConstraintsConsistency(e.ts, number.of.real.variables)) {
    return(" EMPTY SET ")  
  }
  ss <- generateAllSubsetsOfConstraints(e.pi.rank)
  i <- 0
  while ( i < length(ss)) {
    i = i + 1
    constraints <- ss[[i]]
    ts <- e.ts
    ts <- combineConstraints(ts, constraints)
    
    if (!prCheckConstraintsConsistency(ts, number.of.real.variables)) {
      prs <- append(prs, list(rownames(constraints$lhs)))
      ##removing all supersets
      j <- i
      
      while(j < length(ss)) {
        j= j+1
        a <- ss[[j]]
        if (isASubset(constraints, ss[[j]])) {
          
          ss[[j]] <- NULL
          j = j - 1  #Usunieto element tablicy więc trzeba cofnąć wskaźnik, żeby nie ominąć kolejnego elementu
        } 
        
      }
    }
  }
  return(prs)
}


isASubset <- function(candidate.set, superset) {
  
  is.identical = TRUE
  for (i in seq(nrow(candidate.set$lhs))) {
    candidate.row <- list()
    candidate.row$lhs <- candidate.set$lhs[i, ]
    candidate.row$dir <- candidate.set$dir[i]
    candidate.row$rhs <- candidate.set$rhs[i]
    is.in.superset = FALSE
    for (j in seq(nrow(superset$lhs))) {
      superset.row <-list()
      superset.row$lhs <- superset$lhs[j, ]
      superset.row$dir <- superset$dir[j]
      superset.row$rhs <- superset$rhs[j]
      if (identical(candidate.row$lhs, superset.row$lhs) &&
            identical(candidate.row$dir, superset.row$dir) &&
            identical(candidate.row$rhs, superset.row$rhs)) {
        
        is.in.superset = TRUE;
        break
      }
    }
    if (is.in.superset == FALSE) {
      
      return(FALSE)
    }  
  }
  
  return(TRUE)
}

prCheckConstraintsConsistency <- function(model, number.of.real.variables = 0,  eps.position) {
  #check constraints consistency  
  #  model: structure of constraints with the following elements:
  #    model$lhs: matrix - left side of constraints
  #    model$dir: list of operators
  #    model$rhs: matrix - right side of constraints
  
  ret <- solveModel(model, number.of.real.variables, eps.position)
  return(ret$status$code == 0 && ret$objval >= MINEPS)
}

buildConstraintsForRanksPreferentionalReducts <- function(base.constraints, id.of.variant, perf, ranks, alt.vars ) {
  base.constraints.width <- ncol(base.constraints$lhs)
  a.num <- nrow(perf)
  all.constraints <- list()
  number.of.binary.variables = nrow(perf) 
  differences.best = vector()
  differences.worst = vector()
  additional.variables.list.best = c()
  additional.variables.list.worst = c()
  best <- ranks[id.of.variant,2];
  worst <- ranks[id.of.variant,1]
  
  all.binaries.sum = matrix(ncol= (2 * number.of.binary.variables) + 2, nrow=3, data=0)
  number.of.values = getNumberOfValues(perf)  
  
  for ( i in seq(nrow(perf))) {
    if (i != id.of.variant) {
      if (best > 1) {
        difference <- buildVariantsDiffMatrix(alt.vars, id.of.variant, i)
        #difference[number.of.values] = -1
        differences.best <- rbind(differences.best, difference)
        
        additional.variables.best = vector(mode="numeric", length=number.of.binary.variables * 2)
        additional.variables.best[i] = M_BIG_NUMBER
        additional.variables.list.best = rbind(additional.variables.list.best, additional.variables.best)
        all.binaries.sum[1, i] <- 1
        
      }
      if (worst < a.num) {
        difference <- buildVariantsDiffMatrix(alt.vars, i, id.of.variant)
        difference[number.of.values] = -1
        additional.variables.worst = vector(mode="numeric", length=number.of.binary.variables * 2)
        additional.variables.worst[number.of.binary.variables + i] = M_BIG_NUMBER
        
        additional.variables.list.worst = rbind(additional.variables.list.worst,
                                                additional.variables.worst)
        differences.worst <- rbind(differences.worst, difference)
        all.binaries.sum[2, number.of.binary.variables + i] <- 1
        
      }
    } 
  }
  
  binaries.constraints <- list(
    lhs = all.binaries.sum,
    dir = c("<=", "<=", "=="),
    rhs = c(0, 0, 0)
  )
  
  if (best > 1) {
    binaries.constraints$lhs[1, 2 * number.of.binary.variables + 1] <- -a.num
    binaries.constraints$rhs[1]= best - 2
    binaries.constraints$lhs[3, 2 * number.of.binary.variables + 1] <- 1
  } 
  
  if (worst < a.num) {
    binaries.constraints$lhs[2, 2 * number.of.binary.variables + 2] <- -a.num
    binaries.constraints$rhs[2]= a.num - worst - 1
    binaries.constraints$lhs[3, 2 * number.of.binary.variables + 2] <- 1
  }
  if ((worst < a.num) && (best > 1)) {
    binaries.constraints$rhs[3]= 1  
  }
  
  
  if (!is.null(nrow(differences.best))) {
    #Normalizacja best
    differences.best <- getNormalizedMatrix(
      matrix = differences.best,
      width = base.constraints.width
    )
    differences.best <- cbind(differences.best, additional.variables.list.best)
    
  }
  if (!is.null(nrow(differences.worst))) {
    #Normalizacja worst
    differences.worst <- getNormalizedMatrix(
      matrix = differences.worst,
      width = base.constraints.width
    )
    
    differences.worst <- cbind(differences.worst, additional.variables.list.worst) 
  }
  
  differences.constraint.best <- list()
  differences.constraint.worst <- list()
  if (!is.null(nrow(differences.best))) {
    differences.constraint.best <- list(
      lhs = differences.best,  
      dir = rep(">=", nrow(differences.best)),
      rhs = rep( 0, nrow(differences.best))
    )
  }
  if (!is.null(nrow(differences.worst))) {
    differences.constraint.worst <- list(
      lhs = differences.worst,  
      dir = rep(">=", nrow(differences.worst)),
      rhs = rep( 0, nrow(differences.worst))
    )
  }
  
  differences.constraints <- list()
  if ((!is.null(nrow(differences.best))) && (!is.null(nrow(differences.worst)))) {
    differences.constraints <- combineConstraints(differences.constraint.worst, differences.constraint.best )
  } else {
    if (!is.null(nrow(differences.best))) {
      differences.constraints <- differences.constraint.best 
    }
    if (!is.null(nrow(differences.worst))) {
      differences.constraints <- differences.constraint.worst     
    }    
  }
  #print("DIFF CONST")
  width.of.lhs <- 0
  if (is.matrix(differences.constraints$lhs)) {
    width.of.lhs <- ncol(differences.constraints$lhs)
  }
  
  differences.constraints$lhs <- getNormalizedMatrix(
    matrix = differences.constraints$lhs,
    width = width.of.lhs + 2
  )
  if (!is.null(differences.constraints$lhs)) {
    width.of.lhs <- ncol(differences.constraints$lhs)
  }
  #print("BINARY CONST")
  #print(binaries.constraints$lhs)
  
  binaries.constraints$lhs <- getNormalizedMatrix(
    matrix = binaries.constraints$lhs,
    width = width.of.lhs,
    right = FALSE
  )
  #binaries.constraints <- 
  base.constraints$lhs <- getNormalizedMatrix(
    matrix = base.constraints$lhs,
    width = width.of.lhs
  )
  all.constraints <- combineConstraints(base.constraints, differences.constraints, binaries.constraints)
  
  return(all.constraints)
}

findRankRelatedReducts <- function(perf, ranks, strict.vf, strong.prefs = NULL, weak.prefs = NULL,
                                  indif.prefs = NULL, nums.of.characteristic.points = NULL, criteria=NULL,
                                  criteria.by.nodes=NULL, nodeid=NULL) {
  
  
  #Function finds all ranks preferentional reducts
  # ranks:  an n x 2 matrix, where each row describes worst and best rank for variant 
  if (is.null(nums.of.characteristic.points)) {
    nums.of.characteristic.points = rep(0, ncol(perf))
  }
  if (is.null(criteria)) {
    criteria = rep("g", ncol(perf))
  }
  
  filter <- NULL
  if ((!is.null(nodeid)) && (!is.null(criteria.by.nodes))) {
    criteria.set <- criteria.by.nodes[[nodeid]]
    filter <- getCriteriaFilter(perf=perf, criteria=criteria.set)
  }
  alt.vars <- buildAltVariableMatrix(perf)
  if (!is.null(filter)){
    alt.vars <- getFilteredAltVariableMatrix(altVar=alt.vars, filter=filter)
  }
  #print(alt.vars)
  
  number.of.real.variables <- getNumberOfVariables(perf=perf, numbers.of.characteristic.points=nums.of.characteristic.points)
  base.constraints <- buildBaseConstraints(perf, number.of.real.variables, strict.vf, nums.of.characteristic.points, criteria=criteria)
  pairwise.comparison.constraints <- buildPairwiseComparisonConstraints(perf, 
                                                                        strong.prefs, weak.prefs, indif.prefs,
                                                                        criteria.by.nodes=criteria.by.nodes)
  
  pairwise.comparison.constraints$lhs <- getNormalizedMatrix(pairwise.comparison.constraints$lhs, number.of.real.variables);
  reducts <- list();
  
  for(i in seq(nrow(perf))) {
    constraints <- buildConstraintsForRanksPreferentionalReducts( base.constraints, i, perf, ranks, alt.vars )
    pairwise.comparison.constraints$lhs <- getNormalizedMatrix(pairwise.comparison.constraints$lhs, ncol(constraints$lhs));
    #num.of.bin <- 2 * nrow(perf) + 2
    result <-prFindAllPreferentionalReducts(e.ts = constraints , e.pi.rank = pairwise.comparison.constraints, name = paste(i), number.of.real.variables )
    
    if (length(result) == 0) {
      result <- " EMPTY SET "
    }
    #reducts<- append(reducts, list(name = paste(i,"-", ranks[i,1], ":", ranks[i,2]), reduct = result))
    reducts[[paste(rownames(perf)[i], ":[", ranks[i,1], ":", ranks[i,2], "]")]] <- result
  }
  return(reducts)
}




# Funkcja znajduje redukty preferencyjene ( minimalne podzbiory porównań wprowadzonych przez użytkownika ) indukujące
#   dla pary atrybutów a i b, zależność a  jest koniecznie słabo preferowane nad b. Funkcja znajduje takie redukty dla każdej
#   koniecznej słabej preferencji wynikającej z informacji preferencyjnej.
# Przykład wywołania:
#   red <- PrFindPreferentionalReductsForNecessaryRelations(perf=performances, strong.prefs=str, weak.prefs=weak,
#indif.prefs = indif, strict.vf=FALSE)
findReductsForNecRelations <- function(perf, strict.vf=FALSE, strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL, 
                                       nums.of.characteristic.points = NULL, criteria=NULL, nec.relations.matrix=NULL, 
                                       criteria.by.nodes=NULL, nodeid=NULL) {
  
  
  if (is.null(nums.of.characteristic.points)) {
    nums.of.characteristic.points = rep(0, ncol(perf))
  }
  if (is.null(nec.relations.matrix)) {
    return(list())
  }
  if (is.null(criteria)) {
    criteria = rep("g", ncol(perf))
  }

  filter <- NULL
  if ((!is.null(nodeid)) && (!is.null(criteria.by.nodes))) {
    criteria.set <- criteria.by.nodes[[nodeid]]
    filter <- getCriteriaFilter(perf=perf, criteria=criteria.set)
  }
  alt.vars <- buildAltVariableMatrix(perf)
  if (!is.null(filter)){
    alt.vars <- getFilteredAltVariableMatrix(altVar=alt.vars, filter=filter)
  }
  number.of.real.variables <- getNumberOfVariables(perf=perf, numbers.of.characteristic.points=nums.of.characteristic.points)
  base.constraints <- buildBaseConstraints(perf, number.of.real.variables, strict.vf, nums.of.characteristic.points, criteria=criteria)
  pairwise.comparison.constraints <- buildPairwiseComparisonConstraints(perf, strong.prefs, weak.prefs, indif.prefs, criteria.by.nodes=criteria.by.nodes)
  
  reducts <- list();
  
  necessary.relations <- matrixOfRelationsToList(nec.relations.matrix)
  pairwise.comparison.constraints$lhs <- getNormalizedMatrix(pairwise.comparison.constraints$lhs, number.of.real.variables)
  
  if (nrow(necessary.relations) > 0) {
    for(i in seq(nrow(necessary.relations))) {
      all.const <- c()
      relation <- necessary.relations[i, ]
        
      necessary.relation <- buildStrongPreferenceConstraint(relation[2], relation[1], alt.vars) 
      necessary.relation$lhs <- matrix(data = necessary.relation$lhs, nrow = 1)
      row.names(necessary.relation$lhs) <- paste(relation[2], "-", relation[1])
      necessary.relation$lhs <- getNormalizedMatrix(necessary.relation$lhs, number.of.real.variables);
      all.const <- combineConstraints(base.constraints, necessary.relation)
      reducts[[paste(relation[1], ">=^N",relation[2])]] <- prFindAllPreferentionalReducts(
        e.ts = all.const , e.pi.rank = pairwise.comparison.constraints, name = paste(relation[1], ">",relation[2]),
        number.of.real.variables=number.of.real.variables)
    }
  }
  
  for (necrelation in names(reducts)) {
    elements = list()
    for (element in reducts[necrelation] ){
      element <- paste("[", element , "]")
      elements <- paste(elements, " ", element)
    }
  }
  return(reducts)
}


findPreferentionalReductsForNecessaryRelations <- function(perf, strict.vf,
                                                           strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL, 
                                                           nums.of.characteristic.points = NULL, criteria=NULL, nec.relations.matrix=NULL) {
  reducts <- findReductsForNecRelations(perf=perf, strict.vf=strict.vf,
                                        strong.prefs = strong.prefs, weak.prefs = weak.prefs, indif.prefs = indif.prefs, 
                                        nums.of.characteristic.points = nums.of.characteristic.points, criteria=criteria,
                                        nec.relations.matrix=nec.relations.matrix)
  return(reducts)
}

findPreferentionalReductsForNecessaryRelationsHierarchical <- function(perf, 
                                               strict.vf, 
                                               strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                                               nums.of.characteristic.points=NULL, criteria=NULL, nec.relations=NULL, hierarchy.data=NULL) {
  
  results <- list()
  hierarchy.data <- prepareHierarchyData(perf, hierarchy.data)
  err <- NULL
  if (hierarchy.data[["status"]] == "OK") {
    nodes <- hierarchy.data$nodes
    criteria.by.nodes <- hierarchy.data$criteria.by.nodes 
    
    for (node.id in nodes) {
      reducts <- findReductsForNecRelations(perf=perf, strict.vf=strict.vf, strong.prefs = strong.prefs,
                                            weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                            nums.of.characteristic.points = nums.of.characteristic.points, criteria=criteria, nec.relations.matrix=nec.relations[[node.id]], 
                                            criteria.by.nodes=criteria.by.nodes, nodeid=node.id) 

      results[[node.id]] = reducts
    }  
  } 
  
  return(results)
}

findAllRankRelatedPreferentionalReducts <- function(perf, ranks, strict.vf, strong.prefs = NULL, weak.prefs = NULL,
                                                    indif.prefs = NULL,  nums.of.characteristic.points = NULL, criteria=NULL) {
  
  reducts <- findRankRelatedReducts(perf=perf, strict.vf=strict.vf, ranks=ranks, strong.prefs = strong.prefs, weak.prefs = weak.prefs,
                                    indif.prefs = indif.prefs, nums.of.characteristic.points = nums.of.characteristic.points, criteria=criteria)
  return(reducts)
}

findAllRankRelatedPreferentionalReductsHierarchical <- function(perf, ranks,  strict.vf, 
                                                                strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                                                                nums.of.characteristic.points=NULL, criteria=NULL, hierarchy.data=NULL) {
  
  results <- list()
  hierarchy.data <- prepareHierarchyData(perf, hierarchy.data)
  err <- NULL
  if (hierarchy.data[["status"]] == "OK") {
    nodes <- hierarchy.data$nodes
    criteria.by.nodes <- hierarchy.data$criteria.by.nodes 
    
    for (node.id in nodes) {
      #print(node.id)
      reducts <- findRankRelatedReducts(perf=perf, ranks=ranks[[node.id]], strong.prefs = strong.prefs, weak.prefs = weak.prefs,
                                        indif.prefs = indif.prefs, strict.vf=strict.vf, nums.of.characteristic.points = nums.of.characteristic.points, 
                                        criteria=criteria, criteria.by.nodes=criteria.by.nodes, nodeid=node.id)
      
      results[[node.id]] = reducts
    }  
  } 
  
  return(results)
}

