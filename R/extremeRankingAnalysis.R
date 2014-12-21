
eraSolveModel <- function(performances, model, number.of.real.variables) {
  number.of.variables <- ncol(model$lhs)
  number.of.binary.variables <- nrow(performances)
  types <- rep('C', number.of.variables)
  if (number.of.real.variables < number.of.variables) {
    position.of.first.binary.variable <- number.of.real.variables + 1
    
    #position.of.first.binary.variable <- number.of.variables - number.of.binary.variables + 1
    types[position.of.first.binary.variable : number.of.variables] <- 'B'  
  }
  obj <- L_objective(eraBuildObjective(performances, number.of.variables))
  roiConst <- L_constraint(L = model$lhs, dir =model$dir, rhs=model$rhs)
  lp <- OP(objective=obj, constraints=roiConst, maximum=FALSE, types=types)
  res <- ROI_solve(lp, .solver)
  return(res)
}

eraBuildObjective <- function(performances, number.of.all.variables) {
  objective <- rep(0.0, number.of.all.variables)
  number.of.binary.variables <- nrow(performances)
  position.of.first.binary.variable <- number.of.all.variables - number.of.binary.variables + 1
  objective[position.of.first.binary.variable:number.of.all.variables] <- 1
  return(objective)
}

eraBuildConstraints <- function(rank.constraints, performances, is.rank.max = TRUE, filter=NULL){
  rank.constraints.length = ncol(rank.constraints$lhs)
  number.of.binary.variables = nrow(performances) 
  variants <- seq(number.of.binary.variables)
  constraints.list <- list()
  alt.vars <- buildAltVariableMatrix(performances)
  if (!is.null(filter)){
    alt.vars <- getFilteredAltVariableMatrix(altVar=alt.vars, filter=filter)
  }
  
  for (i in variants){
    differences = c()
    additional.variables.list = c()
    for (j in variants) {
      if (i != j) {        
        if (is.rank.max) {
          difference <- buildVariantsDiffMatrix(alt.vars, i,j)
          #difference[GetNumberOfValues(performances)] = -1
        } else {
          difference <- buildVariantsDiffMatrix(alt.vars, j,i)
          difference[getNumberOfValues(performances)] = -1
        }
        
        additional.variables = vector(mode="numeric",
                                      length=number.of.binary.variables)
        additional.variables[j] = M_BIG_NUMBER
        additional.variables.list = rbind(additional.variables.list,
                                          additional.variables)
        differences <- rbind(differences, difference)
      }
    }
    differences <- getNormalizedMatrix(differences, rank.constraints.length)
    constraints <- list(
      "lhs" = cbind(differences, additional.variables.list),
      "dir" = rep(">=", nrow(additional.variables.list)),
      "rhs" = rep( 0, nrow(additional.variables.list))
    )
    all.constraints.length <- ncol(constraints$lhs)
    rank.constraints$lhs <- getNormalizedMatrix(rank.constraints$lhs, all.constraints.length)
    allConstraints <- combineConstraints(rank.constraints, constraints)  
    constraints.list[i] <- list(allConstraints)
    
  } 
  return(constraints.list)
}
#
# Parametry wykorzystywane dla większości funkcji:
#   perf - macierz opisująca warianty (wiersze) i wartości ich atrybutów (kolumny)
#   strong.prefs
#   weak.prefs
#   indif.prefs - macierze o wymiarach n x 2 opisujące preferencje użytkownika ( odpowiednio: preferencję silną, słabą i nierozróżnialność ) 
#     wariant [i, 1] jest preferowany nad wariant [i, 2]
#   strict.vf - zmienna logiczna określająca czy funkcje cząstkowe są silnie czy słabo monotoniczne.
#   nums.of.characteristic.points - lista liczb określających liczbę punktów charakterystycznych na każdym z kryteriów.
#     Liczba 0 lub 1 oznacza funkcje czątkową ogólną. 

# Funkcja znajduje minimalną i maksymalną możliwą pozycję w rankingu dla każdego wariantu (wiersza macierzy) ze struktury performances. 
# Przykład wywołania: 
#   reducts <- ExtremeRankingAnalysis(perf = performances, strong.prefs = str, strict.vf=FALSE)  
extremeRankingAnalysis <- function(perf, 
                             strict.vf, 
                             strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                             strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                             rank.related.requirements = NULL,
                             nums.of.characteristic.points=NULL, criteria=NULL, criteria.by.nodes=NULL, nodeid=NULL) {
  
  # perf - macierz, której wiersze oznaczają różne warianty. Kolumny reprezentują różne atrybuty.
  # strong.prefs
  # weak.prefs
  # indif.prefs - macierze  (n x 2) opisujące odpowiednio (silne, słabe i nierozróżnialne) preferencje użytkownika. 
  #   Wariant [i, 1] jest preferowany nad wariant [i, 2] 
  #    
  #print("era")
  base.model <- buildBaseLPModel(perf, strict.vf, strong.prefs = strong.prefs,
                                 weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                 strong.intensities.of.prefs =  strong.intensities.of.prefs , weak.intensities.of.prefs = weak.intensities.of.prefs,
                                 indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                 rank.related.requirements = rank.related.requirements,
                                 nums.of.characteristic.points=nums.of.characteristic.points, criteria=criteria, 
                                 criteria.by.nodes=criteria.by.nodes)
  
  number.of.real.variables <- getNumberOfVariables(perf=perf, numbers.of.characteristic.points=nums.of.characteristic.points)
  
  eps.position <- getEpsPosition(perf)
  if (!checkConstraintsConsistency(model=base.model, number.of.real.variables=number.of.real.variables, eps.position=eps.position)) {
    stop("Model infeasible")
  } 
  
  variables.set <- NULL
  filter <- NULL
  if ((!is.null(nodeid)) && (!is.null(criteria.by.nodes))) {
    criteria.set <- criteria.by.nodes[[nodeid]]
    filter <- getCriteriaFilter(perf=perf, criteria=criteria.set)
  }
  
  max.rank.constraints <- eraBuildConstraints(base.model, perf, TRUE, filter=filter)
  min.rank.constraints <- eraBuildConstraints(base.model, perf, FALSE, filter=filter)
  
  #print(max.rank.constraints)
  number.of.variants = nrow(perf)
  variants <- seq(number.of.variants)
  ranks = c()
  #print(min.rank.constraints)
  #print(max.rank.constraints)
  
  
  for (i in variants) {
    
    max.result <- eraSolveModel(perf, max.rank.constraints[[i]], number.of.real.variables)
    min.result <- eraSolveModel(perf, min.rank.constraints[[i]], number.of.real.variables)
    if ((max.result$status$code != 0) || (min.result$status$code != 0) ) {
      print("Solution infeasible:")
      #print(i)  
      rank.result <- c(-1, -1)
      ranks <- rbind(ranks, rank.result)
    } else {
      rank.result <- c(number.of.variants - min.result$objval, max.result$objval + 1)
      ranks <- rbind(ranks, rank.result)
      
      #if ((nodeid == "nodes12") && (i == 1)) {
      #  print(max.rank.constraints[[i]])
      #  View(max.rank.constraints[[i]])
      #  print("BASE MODEL")
      #  #print(max.result)
      #  print(max.result$solution)
      #}
    }
  }
  
  rownames(ranks) <- rownames(perf)
  return(ranks)
}

findExtremeRanks <- function(perf, strict.vf, 
                             strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                             strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                             rank.related.requirements = NULL,
                             nums.of.characteristic.points=NULL, criteria=NULL) {

  ranks <- extremeRankingAnalysis(perf=perf, strict.vf=strict.vf, strong.prefs = strong.prefs,
                              weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                              strong.intensities.of.prefs =  strong.intensities.of.prefs , weak.intensities.of.prefs = weak.intensities.of.prefs,
                              indif.intensities.of.prefs = indif.intensities.of.prefs, 
                              rank.related.requirements = rank.related.requirements,
                              nums.of.characteristic.points=nums.of.characteristic.points, criteria=criteria)
  
  return(ranks)
}


findExtremeRanksHierarchical <- function(perf, 
                                   strict.vf, 
                                   strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
                                   strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                   rank.related.requirements = NULL,
                                   nums.of.characteristic.points=NULL, criteria=NULL, hierarchy.data=NULL) {
  
  results <- list()
  hierarchy.data <- prepareHierarchyData(perf, hierarchy.data)
  err <- NULL
  if (hierarchy.data[["status"]] == "OK") {
    nodes <- hierarchy.data$nodes
    criteria.by.nodes <- hierarchy.data$criteria.by.nodes 
    
    for (node.id in nodes) {
      ranks <- extremeRankingAnalysis(perf=perf, strict.vf=strict.vf, strong.prefs = strong.prefs,
                                weak.prefs = weak.prefs, indif.prefs = indif.prefs,
                                strong.intensities.of.prefs =  strong.intensities.of.prefs , weak.intensities.of.prefs = weak.intensities.of.prefs,
                                indif.intensities.of.prefs = indif.intensities.of.prefs, 
                                rank.related.requirements = rank.related.requirements,
                                nums.of.characteristic.points=nums.of.characteristic.points, criteria=criteria, criteria.by.nodes=criteria.by.nodes, nodeid=node.id)
      
      results[[node.id]] = ranks
    }  
  } 
  
  return(results)
}