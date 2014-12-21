getPerformances2 <- function() {
  performances <- matrix(c(1,1,1,1,
                           3,7,3,2,
                           1,10,5,5,
                           5,5,5,5,
                           1,5,5,5,
                           10,10,10,10), 
                         ncol=4, 
                         byrow=TRUE);
  rownames(performances)=c("a1", "a2", "a3", "a4", "a5", "a6");
  colnames(performances)=c("c01", "c02", "c03", "c04");
  return(performances);
}  

getPerformancesCost <- function() {
  performances <- matrix(c(1,10,1,10,
                           3,4,3,9,
                           1,1,5,6,
                           5,6,5,6,
                           1,6,5,6,
                           10,1,10,1), 
                         ncol=4, 
                         byrow=TRUE);
  rownames(performances)=c("a1", "a2", "a3", "a4", "a5", "a6");
  colnames(performances)=c("c01", "c02", "c03", "c04");
  return(performances);
}  


getHierarchyData <- function() {
  hierarchy.tree <- list(nodes = list(nodes1=list(nodes11=list(), nodes12=list()), nodes2=list()))
  criteria.by.nodes <- list(
    nodes11 = list("c01"),
    nodes12 = list("c02"),
    nodes1 = list("c01", "c02"),
    nodes2 = list("c03", "c04"),
    nodes = list("c01", "c02", "c03", "c04"))
  status = "OK"
  return(list("criteria.by.nodes"=criteria.by.nodes, "hierarchy.tree"=hierarchy.tree, "status"=status))
}




test_that("Finding relations acceptabilities indices work properly.", {
  performances <- getPerformances2()
  #hierarchy.data <- getHierarchyData()
  
  strong.preference = matrix(c("a3","a2"),ncol=2, byrow=TRUE)
  weak.preference = matrix(c("a4","a3"),ncol=2, byrow=TRUE)
  
  results <- findRelationsAcceptabilityIndices(perf =performances, 
                                                           strict.vf = TRUE,
                                                           strong.prefs = strong.preference, weak.prefs = weak.preference, indif.prefs = NULL,
                                                           strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                           rank.related.requirements = NULL,
                                                           nums.of.characteristic.points=NULL, criteria=c("g","g","g","g"), num.of.samples=100)
  
 
  #DIAGONAL
  #print(results)  
  expect_that(results[1,1],is_equivalent_to(100))
  expect_that(results[2,2],is_equivalent_to(100))
  expect_that(results[3,3],is_equivalent_to(100))
  expect_that(results[4,4],is_equivalent_to(100))
  expect_that(results[5,5],is_equivalent_to(100))
  expect_that(results[6,6],is_equivalent_to(100))
  
  #FIRST IS WORST
  expect_that(results[1,1],is_equivalent_to(100))
  expect_that(results[2,1],is_equivalent_to(100))
  expect_that(results[3,1],is_equivalent_to(100))
  expect_that(results[4,1],is_equivalent_to(100))
  expect_that(results[5,1],is_equivalent_to(100))
  expect_that(results[6,1],is_equivalent_to(100))
  
  #LAST IS THE BEST
  expect_that(results[6,1],is_equivalent_to(100))
  expect_that(results[6,2],is_equivalent_to(100))
  expect_that(results[6,3],is_equivalent_to(100))
  expect_that(results[6,4],is_equivalent_to(100))
  expect_that(results[6,5],is_equivalent_to(100))
  expect_that(results[6,6],is_equivalent_to(100))
  
  #(a3 better than a5) and (a4 better than a5)
  expect_that(results[3,5],is_equivalent_to(100))
  expect_that(results[4,5],is_equivalent_to(100))
  #PREFERENCES
  expect_that(results[4,3],is_equivalent_to(100))
  expect_that(results[3,2],is_equivalent_to(100))
  expect_that(results[3,2],is_equivalent_to(100))
    
  #DIAGONAL
  expect_that(results[1,1],is_equivalent_to(100))
  expect_that(results[2,2],is_equivalent_to(100))
  expect_that(results[3,3],is_equivalent_to(100))
  expect_that(results[4,4],is_equivalent_to(100))
  expect_that(results[5,5],is_equivalent_to(100))
  expect_that(results[6,6],is_equivalent_to(100))  
  #FIRST IS WORST
  expect_that(results[1,2],is_equivalent_to(0))
  expect_that(results[1,3],is_equivalent_to(0))
  expect_that(results[1,4],is_equivalent_to(0))
  expect_that(results[1,5],is_equivalent_to(0))
  expect_that(results[1,6],is_equivalent_to(0))
  #LAST IS THE BEST
  expect_that(results[1,6],is_equivalent_to(0))
  expect_that(results[2,6],is_equivalent_to(0))
  expect_that(results[3,6],is_equivalent_to(0))
  expect_that(results[4,6],is_equivalent_to(0))
  expect_that(results[5,6],is_equivalent_to(0))
  
  expect_that(results[5,3],is_equivalent_to(0))
  expect_that(results[5,4],is_equivalent_to(0))
    
  #print("Finding neccessary and possible relations OK")
})

test_that("Finding relations acceptabilities indices work properly - characteristic points.", {
  performances <- getPerformances2()
  #hierarchy.data <- getHierarchyData()
  
  strong.preference = matrix(c("a3","a2"),ncol=2, byrow=TRUE)
  weak.preference = matrix(c("a4","a3"),ncol=2, byrow=TRUE)
  
  results <- findRelationsAcceptabilityIndices(perf =performances, 
                                               strict.vf = TRUE,
                                               strong.prefs = strong.preference, weak.prefs = weak.preference, indif.prefs = NULL,
                                               strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                               rank.related.requirements = NULL,
                                               nums.of.characteristic.points=c(2,2,2,2), criteria=c("g","g","g","g"), num.of.samples=100)
  
  
  #DIAGONAL
  #print(results)  
  expect_that(results[1,1],is_equivalent_to(100))
  expect_that(results[2,2],is_equivalent_to(100))
  expect_that(results[3,3],is_equivalent_to(100))
  expect_that(results[4,4],is_equivalent_to(100))
  expect_that(results[5,5],is_equivalent_to(100))
  expect_that(results[6,6],is_equivalent_to(100))
  
  #FIRST IS WORST
  expect_that(results[1,1],is_equivalent_to(100))
  expect_that(results[2,1],is_equivalent_to(100))
  expect_that(results[3,1],is_equivalent_to(100))
  expect_that(results[4,1],is_equivalent_to(100))
  expect_that(results[5,1],is_equivalent_to(100))
  expect_that(results[6,1],is_equivalent_to(100))
  
  #LAST IS THE BEST
  expect_that(results[6,1],is_equivalent_to(100))
  expect_that(results[6,2],is_equivalent_to(100))
  expect_that(results[6,3],is_equivalent_to(100))
  expect_that(results[6,4],is_equivalent_to(100))
  expect_that(results[6,5],is_equivalent_to(100))
  expect_that(results[6,6],is_equivalent_to(100))
  
  #(a3 better than a5) and (a4 better than a5)
  expect_that(results[3,5],is_equivalent_to(100))
  expect_that(results[4,5],is_equivalent_to(100))
  #PREFERENCES
  expect_that(results[4,3],is_equivalent_to(100))
  expect_that(results[3,2],is_equivalent_to(100))
  expect_that(results[3,2],is_equivalent_to(100))
  
  #DIAGONAL
  expect_that(results[1,1],is_equivalent_to(100))
  expect_that(results[2,2],is_equivalent_to(100))
  expect_that(results[3,3],is_equivalent_to(100))
  expect_that(results[4,4],is_equivalent_to(100))
  expect_that(results[5,5],is_equivalent_to(100))
  expect_that(results[6,6],is_equivalent_to(100))  
  #FIRST IS WORST
  expect_that(results[1,2],is_equivalent_to(0))
  expect_that(results[1,3],is_equivalent_to(0))
  expect_that(results[1,4],is_equivalent_to(0))
  expect_that(results[1,5],is_equivalent_to(0))
  expect_that(results[1,6],is_equivalent_to(0))
  #LAST IS THE BEST
  expect_that(results[1,6],is_equivalent_to(0))
  expect_that(results[2,6],is_equivalent_to(0))
  expect_that(results[3,6],is_equivalent_to(0))
  expect_that(results[4,6],is_equivalent_to(0))
  expect_that(results[5,6],is_equivalent_to(0))
  
  expect_that(results[5,3],is_equivalent_to(0))
  expect_that(results[5,4],is_equivalent_to(0))
  
  #print("Finding neccessary and possible relations OK")
})




test_that("Finding relations acceptabilities indices work properly - characteristic points - cost.", {
  performances <- getPerformancesCost()
  #hierarchy.data <- getHierarchyData()
  
  strong.preference = matrix(c("a3","a2"),ncol=2, byrow=TRUE)
  weak.preference = matrix(c("a4","a3"),ncol=2, byrow=TRUE)
  
  results <- findRelationsAcceptabilityIndices(perf =performances, 
                                               strict.vf = TRUE,
                                               strong.prefs = strong.preference, weak.prefs = weak.preference, indif.prefs = NULL,
                                               strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                               rank.related.requirements = NULL,
                                               nums.of.characteristic.points=c(2,2,2,2), criteria=c("g","c","g","c"), num.of.samples=100)
  
  
  #DIAGONAL
  #print(results)  
  expect_that(results[1,1],is_equivalent_to(100))
  expect_that(results[2,2],is_equivalent_to(100))
  expect_that(results[3,3],is_equivalent_to(100))
  expect_that(results[4,4],is_equivalent_to(100))
  expect_that(results[5,5],is_equivalent_to(100))
  expect_that(results[6,6],is_equivalent_to(100))
  
  #FIRST IS WORST
  expect_that(results[1,1],is_equivalent_to(100))
  expect_that(results[2,1],is_equivalent_to(100))
  expect_that(results[3,1],is_equivalent_to(100))
  expect_that(results[4,1],is_equivalent_to(100))
  expect_that(results[5,1],is_equivalent_to(100))
  expect_that(results[6,1],is_equivalent_to(100))
  
  #LAST IS THE BEST
  expect_that(results[6,1],is_equivalent_to(100))
  expect_that(results[6,2],is_equivalent_to(100))
  expect_that(results[6,3],is_equivalent_to(100))
  expect_that(results[6,4],is_equivalent_to(100))
  expect_that(results[6,5],is_equivalent_to(100))
  expect_that(results[6,6],is_equivalent_to(100))
  
  #(a3 better than a5) and (a4 better than a5)
  expect_that(results[3,5],is_equivalent_to(100))
  expect_that(results[4,5],is_equivalent_to(100))
  #PREFERENCES
  expect_that(results[4,3],is_equivalent_to(100))
  expect_that(results[3,2],is_equivalent_to(100))
  expect_that(results[3,2],is_equivalent_to(100))
  
  #DIAGONAL
  expect_that(results[1,1],is_equivalent_to(100))
  expect_that(results[2,2],is_equivalent_to(100))
  expect_that(results[3,3],is_equivalent_to(100))
  expect_that(results[4,4],is_equivalent_to(100))
  expect_that(results[5,5],is_equivalent_to(100))
  expect_that(results[6,6],is_equivalent_to(100))  
  #FIRST IS WORST
  expect_that(results[1,2],is_equivalent_to(0))
  expect_that(results[1,3],is_equivalent_to(0))
  expect_that(results[1,4],is_equivalent_to(0))
  expect_that(results[1,5],is_equivalent_to(0))
  expect_that(results[1,6],is_equivalent_to(0))
  #LAST IS THE BEST
  expect_that(results[1,6],is_equivalent_to(0))
  expect_that(results[2,6],is_equivalent_to(0))
  expect_that(results[3,6],is_equivalent_to(0))
  expect_that(results[4,6],is_equivalent_to(0))
  expect_that(results[5,6],is_equivalent_to(0))
  
  expect_that(results[5,3],is_equivalent_to(0))
  expect_that(results[5,4],is_equivalent_to(0))
  
  #print("Finding neccessary and possible relations OK")
})


test_that("Finding relations acceptabilities indices work properly - characteristic points - cost.", {
  performances <- getPerformancesCost()
  #hierarchy.data <- getHierarchyData()
  
  strong.preference = matrix(c("a3","a2"),ncol=2, byrow=TRUE)
  weak.preference = matrix(c("a4","a3"),ncol=2, byrow=TRUE)
  
  results <- findRelationsAcceptabilityIndices(perf =performances, 
                                               strict.vf = TRUE,
                                               strong.prefs = strong.preference, weak.prefs = weak.preference, indif.prefs = NULL,
                                               strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                               rank.related.requirements = NULL,
                                               nums.of.characteristic.points=c(), criteria=c("g","c","g","c"), num.of.samples=100)
  
  
  #DIAGONAL
  #print(results)  
  expect_that(results[1,1],is_equivalent_to(100))
  expect_that(results[2,2],is_equivalent_to(100))
  expect_that(results[3,3],is_equivalent_to(100))
  expect_that(results[4,4],is_equivalent_to(100))
  expect_that(results[5,5],is_equivalent_to(100))
  expect_that(results[6,6],is_equivalent_to(100))
  
  #FIRST IS WORST
  expect_that(results[1,1],is_equivalent_to(100))
  expect_that(results[2,1],is_equivalent_to(100))
  expect_that(results[3,1],is_equivalent_to(100))
  expect_that(results[4,1],is_equivalent_to(100))
  expect_that(results[5,1],is_equivalent_to(100))
  expect_that(results[6,1],is_equivalent_to(100))
  
  #LAST IS THE BEST
  expect_that(results[6,1],is_equivalent_to(100))
  expect_that(results[6,2],is_equivalent_to(100))
  expect_that(results[6,3],is_equivalent_to(100))
  expect_that(results[6,4],is_equivalent_to(100))
  expect_that(results[6,5],is_equivalent_to(100))
  expect_that(results[6,6],is_equivalent_to(100))
  
  #(a3 better than a5) and (a4 better than a5)
  expect_that(results[3,5],is_equivalent_to(100))
  expect_that(results[4,5],is_equivalent_to(100))
  #PREFERENCES
  expect_that(results[4,3],is_equivalent_to(100))
  expect_that(results[3,2],is_equivalent_to(100))
  expect_that(results[3,2],is_equivalent_to(100))
  
  #DIAGONAL
  expect_that(results[1,1],is_equivalent_to(100))
  expect_that(results[2,2],is_equivalent_to(100))
  expect_that(results[3,3],is_equivalent_to(100))
  expect_that(results[4,4],is_equivalent_to(100))
  expect_that(results[5,5],is_equivalent_to(100))
  expect_that(results[6,6],is_equivalent_to(100))  
  #FIRST IS WORST
  expect_that(results[1,2],is_equivalent_to(0))
  expect_that(results[1,3],is_equivalent_to(0))
  expect_that(results[1,4],is_equivalent_to(0))
  expect_that(results[1,5],is_equivalent_to(0))
  expect_that(results[1,6],is_equivalent_to(0))
  #LAST IS THE BEST
  expect_that(results[1,6],is_equivalent_to(0))
  expect_that(results[2,6],is_equivalent_to(0))
  expect_that(results[3,6],is_equivalent_to(0))
  expect_that(results[4,6],is_equivalent_to(0))
  expect_that(results[5,6],is_equivalent_to(0))
  
  expect_that(results[5,3],is_equivalent_to(0))
  expect_that(results[5,4],is_equivalent_to(0))
  
  #print("Finding neccessary and possible relations OK")
})

test_that("Finding necessary and possible relations work properly - Hierarchical.", {
  performances <- getPerformances2()
  hierarchy.data <- getHierarchyData()
  strong.preference = matrix(c("a2","a4", "nodes1"), ncol=3,byrow=TRUE)  
  
  results <- findRelationsAcceptabilityIndicesHierarchical(perf =performances, 
                                               strict.vf = TRUE,
                                               strong.prefs = strong.preference, weak.prefs = NULL, indif.prefs = NULL,
                                               strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                               rank.related.requirements = NULL,
                                               nums.of.characteristic.points=c(), criteria=c("g","g","g","g"), num.of.samples=50, hierarchy.data=hierarchy.data)
  
  #DIAGONAL
  expect_that(results$nodes1[2,4],is_equivalent_to(100))
  expect_that(results$nodes1[4,2],is_equivalent_to(0))
  expect_that(results$nodes1[4,5],is_equivalent_to(100))
  expect_that(results$nodes1[5,4],is_equivalent_to(0))
  #print("Finding neccessary and possible relations - Hierarchical OK")
})

test_that("Finding necessary and possible relations work properly - Hierarchical - cost.", {
  performances <- getPerformances2()
  hierarchy.data <- getHierarchyData()
  strong.preference =  matrix(c("a2","a4"), ncol=2,byrow=TRUE)  
  
  results <- findRelationsAcceptabilityIndicesHierarchical(perf =performances, 
                                                           strict.vf = TRUE,
                                                           strong.prefs = strong.preference, weak.prefs = NULL, indif.prefs = NULL,
                                                           strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                           rank.related.requirements = NULL,
                                                           nums.of.characteristic.points=c(), criteria=c("g","c","g","c"), num.of.samples=50, hierarchy.data=hierarchy.data)
  
  #DIAGONAL
  expect_that(results$nodes1[4,2],is_equivalent_to(100))
  expect_that(results$nodes1[2,4],is_equivalent_to(0))
  expect_that(results$nodes1[4,5],is_equivalent_to(100))
  expect_that(results$nodes1[5,4],is_equivalent_to(0))
  #print("Finding neccessary and possible relations - Hierarchical OK")
})