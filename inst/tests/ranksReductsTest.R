library(testthat)

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


test_that("Finding rank related reducts work properly. ", {
  
  performances <- matrix(c(1,1,1,1,
                           9,2,3,1,
                           5,5,5,1,
                           9,1,5,1,
                           5,5,3,1,
                           10,10,10,1), 
                         ncol=4, 
                         byrow=TRUE);
  rownames(performances)=c("a1", "a2", "a3", "a4", "a5", "a6");
  colnames(performances)=c("c01", "c02", "c03", "c04");
  
  strong.preference = matrix(c("a4","a3"), ncol=2,byrow=TRUE)  
  #indif.preference = matrix(c("a5","a3"), ncol=2,byrow=TRUE)  
  
  
  ranks <- findExtremeRanks(perf = performances, 
                                  strict.vf = FALSE, 
                                  strong.prefs = strong.preference, weak.prefs = NULL, indif.prefs = NULL,
                                  nums.of.characteristic.points=NULL)
  
  
  #print(ranks)
  
  reducts <- findAllRankRelatedPreferentionalReducts(perf=performances, ranks=ranks, strict.vf=FALSE, strong.prefs = strong.preference, weak.prefs = NULL,
                                                     indif.prefs = NULL,  nums.of.characteristic.points = NULL)
  
  expect_that(reducts[["a1 :[ 6 : 4 ]"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts[["a2 :[ 4 : 1 ]"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts[["a3 :[ 4 : 3 ]"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts[["a4 :[ 3 : 1 ]"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts[["a5 :[ 5 : 4 ]"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts[["a6 :[ 1 : 1 ]"]][[1]],is_equivalent_to(" EMPTY SET "))
})

test_that("Finding rank related reducts work properly - hierarchical version. ", {
  
  performances <- matrix(c(1,1,1,1,
                           9,2,3,1,
                           5,5,5,1,
                           9,1,5,1,
                           5,5,3,1,
                           10,10,10,1), 
                         ncol=4, 
                         byrow=TRUE);
  rownames(performances)=c("a1", "a2", "a3", "a4", "a5", "a6");
  colnames(performances)=c("c01", "c02", "c03", "c04");
  hierarchy.data <- getHierarchyData()
  
  strong.preference = matrix(c("a4","a3"), ncol=2,byrow=TRUE)  
  #indif.preference = matrix(c("a5","a3"), ncol=2,byrow=TRUE)  
  
  
  ranks <- findExtremeRanksHierarchical(perf = performances, 
                                              strict.vf = FALSE, 
                                              strong.prefs = strong.preference, weak.prefs = NULL, indif.prefs = NULL,
                                              nums.of.characteristic.points=NULL, hierarchy.data=hierarchy.data)
  
  

  
  reducts <- findAllRankRelatedPreferentionalReductsHierarchical(perf=performances, ranks=ranks, strict.vf=FALSE, strong.prefs = strong.preference, weak.prefs = NULL,
                                                                 indif.prefs = NULL,  nums.of.characteristic.points = NULL, hierarchy.data =hierarchy.data)
  #nodes
  expect_that(reducts$nodes[["a1 :[ 6 : 4 ]"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts$nodes[["a2 :[ 4 : 1 ]"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts$nodes[["a3 :[ 4 : 3 ]"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts$nodes[["a4 :[ 3 : 1 ]"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts$nodes[["a5 :[ 5 : 4 ]"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts$nodes[["a6 :[ 1 : 1 ]"]][[1]],is_equivalent_to(" EMPTY SET "))
  #nodes1  
  expect_that(reducts$nodes1[["a1 :[ 6 : 4 ]"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts$nodes1[["a2 :[ 2 : 1 ]"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts$nodes1[["a3 :[ 4 : 4 ]"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts$nodes1[["a4 :[ 3 : 1 ]"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts$nodes1[["a5 :[ 4 : 4 ]"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts$nodes1[["a6 :[ 1 : 1 ]"]][[1]],is_equivalent_to(" EMPTY SET "))
  
  strong.preference =  matrix(c("a4","a3", "nodes1"), ncol=3,byrow=TRUE)  
  indif.preference = matrix(c("a5","a3", "nodes2"), ncol=3,byrow=TRUE)
  hierarchy.data <- getHierarchyData()
  
  ranks <- findExtremeRanksHierarchical(perf = performances, 
                                              strict.vf = FALSE, 
                                              strong.prefs = strong.preference, weak.prefs = NULL, indif.prefs = indif.preference,
                                              nums.of.characteristic.points=NULL, hierarchy.data=hierarchy.data)
    
  reducts <- findAllRankRelatedPreferentionalReductsHierarchical(perf=performances, ranks=ranks, strict.vf=FALSE, strong.prefs = strong.preference, weak.prefs = NULL,
                                                                 indif.prefs =  indif.preference,  nums.of.characteristic.points = NULL, hierarchy.data =hierarchy.data)
  
  #nodes  
  expect_that(reducts$nodes[["a1 :[ 6 : 4 ]"]][[1]][[1]],is_equivalent_to("a4 > a3[nodes1]"))
  expect_that(reducts$nodes[["a2 :[ 2 : 1 ]"]][[1]][[1]],is_equivalent_to("a4 > a3[nodes1]"))
  expect_that(reducts$nodes[["a2 :[ 2 : 1 ]"]][[1]][[2]],is_equivalent_to("a5 == a3[nodes2]"))
  expect_that(reducts$nodes[["a3 :[ 4 : 4 ]"]][[1]][[1]],is_equivalent_to("a4 > a3[nodes1]"))
  expect_that(reducts$nodes[["a3 :[ 4 : 4 ]"]][[1]][[2]],is_equivalent_to("a5 == a3[nodes2]"))
  expect_that(reducts$nodes[["a4 :[ 3 : 1 ]"]][[1]][[1]],is_equivalent_to("a4 > a3[nodes1]"))
  expect_that(reducts$nodes[["a5 :[ 4 : 4 ]"]][[1]][[1]],is_equivalent_to("a4 > a3[nodes1]"))
  expect_that(reducts$nodes[["a5 :[ 4 : 4 ]"]][[1]][[2]],is_equivalent_to("a5 == a3[nodes2]"))
  expect_that(reducts$nodes[["a6 :[ 1 : 1 ]"]][[1]],is_equivalent_to(" EMPTY SET "))
  
  #nodes2  
  expect_that(reducts$nodes2[["a1 :[ 6 : 1 ]"]][[1]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts$nodes2[["a2 :[ 2 : 1 ]"]][[1]][[1]],is_equivalent_to("a5 == a3[nodes2]"))
  expect_that(reducts$nodes2[["a3 :[ 2 : 1 ]"]][[1]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts$nodes2[["a4 :[ 2 : 1 ]"]][[1]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts$nodes2[["a5 :[ 2 : 1 ]"]][[1]][[1]],is_equivalent_to("a5 == a3[nodes2]"))
  expect_that(reducts$nodes2[["a6 :[ 1 : 1 ]"]][[1]],is_equivalent_to(" EMPTY SET "))
  
})