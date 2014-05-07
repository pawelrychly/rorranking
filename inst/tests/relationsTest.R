library(testthat)
getPerformances0 <- function() {
  performances <- matrix(c(1,1,
                           1,5,
                           5,1,
                           5,5), 
                         ncol=2, 
                         byrow=TRUE);
  rownames(performances)=c("a1", "a2", "a3", "a4");
  colnames(performances)=c("g1", "g2");
  return(performances);
}

getPerformances1 <- function() {
  performances <- matrix(c(1,1,
                           5,1,
                           1,5,
                           5,5,
                           10,10), 
                         ncol=2,
                         byrow=TRUE);
  rownames(performances)=c("a1", "a2", "a3", "a4", "a5");
  colnames(performances)=c("g1", "g2");
  return(performances);
}

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

getPerformancesHierarchical <- function() {
  performances <- matrix(c(5,1,2,1,
                           1,5,1,2,
                           5,1,5,5,
                           5,5,5,5,
                           1,5,10,10,
                           10,10,10,10), 
                         ncol=4, 
                         byrow=TRUE);
  rownames(performances)=c("a1", "a2", "a3", "a4", "a5", "a6");
  colnames(performances)=c("c01", "c02", "c03", "c04");
  return(performances);
}

getPerformancesHierarchical2 <- function() {
  performances <- matrix(c(1,1,2,1,
                           2,1,1,2,
                           3,1,5,5,
                           1,2,5,5,
                           1,3,10,10,
                           1,4,10,10), 
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


test_that("Finding necessary and possible relations work properly.", {
  performances <- getPerformances2()
  hierarchy.data <- getHierarchyData()
  
  strong.preference = matrix(c("a3","a2"),ncol=2, byrow=TRUE)
  weak.preference = matrix(c("a4","a3"),ncol=2, byrow=TRUE)
  results <- findNecessaryAndPossiblePreferenceRelations(perf =performances, 
                                                         strict.vf = TRUE,
                                                         strong.prefs = strong.preference, weak.prefs = weak.preference, indif.prefs = NULL,
                                                         strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                         rank.related.requirements = NULL,
                                                         nums.of.characteristic.points=NULL)
  
  nec.relations = results$nec.relations
  pos.relations = results$pos.relations
  #DIAGONAL
  expect_that(nec.relations[1,1],is_equivalent_to(TRUE))
  expect_that(nec.relations[2,2],is_equivalent_to(TRUE))
  expect_that(nec.relations[3,3],is_equivalent_to(TRUE))
  expect_that(nec.relations[4,4],is_equivalent_to(TRUE))
  expect_that(nec.relations[5,5],is_equivalent_to(TRUE))
  expect_that(nec.relations[6,6],is_equivalent_to(TRUE))
  
  #FIRST IS WORST
  expect_that(nec.relations[1,1],is_equivalent_to(TRUE))
  expect_that(nec.relations[2,1],is_equivalent_to(TRUE))
  expect_that(nec.relations[3,1],is_equivalent_to(TRUE))
  expect_that(nec.relations[4,1],is_equivalent_to(TRUE))
  expect_that(nec.relations[5,1],is_equivalent_to(TRUE))
  expect_that(nec.relations[6,1],is_equivalent_to(TRUE))
  
  #LAST IS THE BEST
  expect_that(nec.relations[6,1],is_equivalent_to(TRUE))
  expect_that(nec.relations[6,2],is_equivalent_to(TRUE))
  expect_that(nec.relations[6,3],is_equivalent_to(TRUE))
  expect_that(nec.relations[6,4],is_equivalent_to(TRUE))
  expect_that(nec.relations[6,5],is_equivalent_to(TRUE))
  expect_that(nec.relations[6,6],is_equivalent_to(TRUE))
  
  #(a3 better than a5) and (a4 better than a5)
  expect_that(nec.relations[3,5],is_equivalent_to(TRUE))
  expect_that(nec.relations[4,5],is_equivalent_to(TRUE))
  #PREFERENCES
  expect_that(nec.relations[4,3],is_equivalent_to(TRUE))
  expect_that(nec.relations[3,2],is_equivalent_to(TRUE))
  expect_that(nec.relations[3,2],is_equivalent_to(TRUE))
  
  
  #DIAGONAL
  expect_that(pos.relations[1,1],is_equivalent_to(TRUE))
  expect_that(pos.relations[2,2],is_equivalent_to(TRUE))
  expect_that(pos.relations[3,3],is_equivalent_to(TRUE))
  expect_that(pos.relations[4,4],is_equivalent_to(TRUE))
  expect_that(pos.relations[5,5],is_equivalent_to(TRUE))
  expect_that(pos.relations[6,6],is_equivalent_to(TRUE))  
  
  #FIRST IS WORST
  expect_that(pos.relations[1,2],is_equivalent_to(FALSE))
  expect_that(pos.relations[1,3],is_equivalent_to(FALSE))
  expect_that(pos.relations[1,4],is_equivalent_to(FALSE))
  expect_that(pos.relations[1,5],is_equivalent_to(FALSE))
  expect_that(pos.relations[1,6],is_equivalent_to(FALSE))
  
  #LAST IS THE BEST
  expect_that(pos.relations[1,6],is_equivalent_to(FALSE))
  expect_that(pos.relations[2,6],is_equivalent_to(FALSE))
  expect_that(pos.relations[3,6],is_equivalent_to(FALSE))
  expect_that(pos.relations[4,6],is_equivalent_to(FALSE))
  expect_that(pos.relations[5,6],is_equivalent_to(FALSE))
  
  expect_that(pos.relations[5,3],is_equivalent_to(FALSE))
  expect_that(pos.relations[5,4],is_equivalent_to(FALSE))
  
  
  #print("Finding neccessary and possible relations OK")
})

test_that("Finding necessary and possible relations work properly - Hierarchical.", {
  performances <- getPerformances2()
  hierarchy.data <- getHierarchyData()
  strong.preference = matrix(c("a2","a4", "nodes1"), ncol=3,byrow=TRUE)  
  
  results <- findNecessaryAndPossiblePreferenceRelationsHierarchical(
    perf = performances, strict.vf = TRUE, strong.prefs = strong.preference,nums.of.characteristic.points=c(), hierarchy.data=hierarchy.data
  ) 
  
  nodes1.nec <- results$nec.relations$nodes1
  nodes1.pos <- results$pos.relations$nodes1
  
  #DIAGONAL
  expect_that(nodes1.nec[2,4],is_equivalent_to(TRUE))
  expect_that(nodes1.pos[4,2],is_equivalent_to(FALSE))
  
  #print("Finding neccessary and possible relations - Hierarchical OK")
})
