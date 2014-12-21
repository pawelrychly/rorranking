library(testthat)
library(rorranking)
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
  colnames(performances)=c("g1", "g2", "g3", "g4");
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

getPerformancesHierarchicalInverted <- function() {
  performances <- matrix(c(6,10,9,10,
                           10,6,10,9,
                           6,10,6,6,
                           6,6,6,6,
                           10,6,1,1,
                           1,1,1,1), 
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

getPerformancesHierarchical2Inverted <- function() {
  performances <- matrix(c(10,10,9,10,
                           9,10,10,9,
                           8,10,6,6,
                           10,9,6,6,
                           10,8,1,1,
                           10,7,1,1), 
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


test_that("ERA hierarchical structure work properly for extreme rankings", {
  performances <- getPerformancesHierarchical()
  hierarchy.data <- getHierarchyData()
  strong.preference.intensities = matrix(c("a4","a5", "a4","a3", "nodes1"),ncol=5, byrow=TRUE) #kryterium 1 ważniejsze
  strong.preference = matrix(c("a1","a2", "nodes2"),ncol=3, byrow=TRUE)
  
  #preference.intensities = matrix(c("a3",, 4,2,0),ncol=5, byrow=TRUE)
  #print(preference.intensities)
  #print(performances)
  results <- findExtremeRanksHierarchical(perf = performances, 
                                                strict.vf = TRUE, 
                                                strong.prefs = strong.preference, 
                                                strong.intensities.of.prefs = strong.preference.intensities, 
                                                rank.related.requirements = NULL,nums.of.characteristic.points=c(), hierarchy.data = hierarchy.data) 
  
  nodes1.ranks = results$nodes1
  nodes2.ranks = results$nodes2
  nodes.ranks = results$nodes
  expect_that(nodes1.ranks[2,1],is_more_than(nodes1.ranks[1,2]))
  expect_that(nodes2.ranks[6,1],is_equivalent_to(nodes2.ranks[5,1]))
  expect_that(nodes2.ranks[2,2],is_more_than(nodes2.ranks[1,1]))
  indif.preference.intensities = matrix(c("a4","a3", "a4","a5", "nodes1"),ncol=5, byrow=TRUE)
  indif.preference = matrix(c("a1","a2", "nodes2"),ncol=3, byrow=TRUE)
 
  results <- findExtremeRanksHierarchical(perf = performances, 
                                                strict.vf = TRUE, 
                                                indif.prefs = indif.preference, 
                                                indif.intensities.of.prefs = indif.preference.intensities, 
                                                rank.related.requirements = NULL,nums.of.characteristic.points=c(), hierarchy.data = hierarchy.data) 
  nodes1.ranks = results$nodes1
  nodes2.ranks = results$nodes2
  
  expect_that(nodes1.ranks[2,2],is_equivalent_to(nodes1.ranks[1,2]))
  expect_that(nodes1.ranks[2,1],is_equivalent_to(nodes1.ranks[1,1]))
  expect_that(nodes2.ranks[1,2],is_equivalent_to(nodes2.ranks[2,2]))
  expect_that(nodes2.ranks[1,1],is_equivalent_to(nodes2.ranks[2,1]))
  
  weak.preference = matrix(c("a1","a2", "nodes2"),ncol=3, byrow=TRUE)
  weak.preference.intensities = matrix(c("a4","a3", "a4","a5", "nodes1"),ncol=5, byrow=TRUE)
  
  results <- findExtremeRanksHierarchical(perf = performances, 
                                                strict.vf = TRUE, 
                                                weak.prefs = weak.preference, 
                                                weak.intensities.of.prefs =  weak.preference.intensities, 
                                                rank.related.requirements = NULL,nums.of.characteristic.points=c(), hierarchy.data = hierarchy.data) 
  nodes1.ranks = results$nodes1
  nodes2.ranks = results$nodes2
  expect_that(nodes1.ranks[1,2] + 1,is_more_than(nodes1.ranks[2,1]))  #x+1 > y: x greater or equal y
  expect_that(nodes2.ranks[2,1] + 1,is_more_than(nodes2.ranks[1,1]))  #x+1 > y: x greater or equal y
  
  #RANK RELATED REQUIREMENTS
  c(1,1,2,1,
    2,1,1,2,
    3,1,5,5,
    1,2,5,5,
    1,3,10,10,
    1,4,10,10)
  rank.related <- matrix(c("a2",2,2, "nodes1"),ncol=4, byrow=TRUE)
  results <- findExtremeRanksHierarchical(perf = getPerformancesHierarchical2(), 
                                                strict.vf = TRUE, 
                                                rank.related.requirements = rank.related,nums.of.characteristic.points=c(), hierarchy.data = hierarchy.data) 
  nodes1.ranks = results$nodes1
  nodes2.ranks = results$nodes2
  nodes.ranks = results$nodes
  
  expect_that(nodes1.ranks[2,2],is_equivalent_to(2))  
  expect_that(nodes1.ranks[2,1],is_equivalent_to(2))  
  expect_that(nodes1.ranks[3,2],is_equivalent_to(1))  
  expect_that(nodes1.ranks[3,1],is_equivalent_to(1))  
  #print("ERA hierarchical structure work properly for extreme rankings OK")
})


test_that("ERA Strong preference affects properly on the extreeme rankings - no characteristic points", {
  performances <- getPerformances2()
  #if 5 > 2 then 3 > 2
  strong.preference = matrix(c(5,2), ncol=2,byrow=TRUE)  
  ranks <- findExtremeRanks(perf = performances, 
                                  strict.vf = TRUE, 
                                  strong.prefs = strong.preference, 
                                  strong.intensities.of.prefs = NULL, 
                                  rank.related.requirements = NULL,nums.of.characteristic.points=c()) 
  expect_that(ranks[2,2],is_more_than(ranks[5,1]))
  expect_that(ranks[2,2],is_more_than(ranks[3,1]))
  #print("ERA strong preference OK")
})

test_that("ERA strong preference affects properly on the extreeme rankings - two characteristic points per function", {
  performances <- getPerformances2()
  #if 5 > 2 then 3 > 2
  strong.preference = matrix(c(5,2), ncol=2,byrow=TRUE)  
  ranks <- findExtremeRanks(perf = performances, 
                                  strict.vf = TRUE, 
                                  strong.prefs = strong.preference, 
                                  strong.intensities.of.prefs = NULL, 
                                  rank.related.requirements = NULL,nums.of.characteristic.points=c(2,2,2,2)) 
  expect_that(ranks[2,2],is_more_than(ranks[5,1]))
  expect_that(ranks[2,2],is_more_than(ranks[3,1]))
  #print("ERA strong preference with characteristic points OK")
})

test_that("ERA preference intensities affects properly on the extreeme rankings - two characteristic points per function", {
  performances <- getPerformances0()
  #if (4 > 3) > (4 > 2) then property 2 is more important and variant (1,5) is better than (5,1)
  #It is expected that worst position of a2 is lower than best position of a3
  preference.intensities = matrix(c(4,3, 4,2,0),ncol=5, byrow=TRUE)
  #print(performances)
  ranks <- findExtremeRanks(perf = performances, 
                                  strict.vf = TRUE, 
                                  strong.prefs = NULL, 
                                  strong.intensities.of.prefs = preference.intensities, 
                                  rank.related.requirements = NULL,nums.of.characteristic.points=c(2,2)) 
  expect_that(ranks[2,1],is_less_than(ranks[3,2]))
  #print(ranks)
  #print("ERA intensity preference with characteristic points OK")
})

test_that("ERA rank related preferences affects properly on the extreeme rankings", {
  performances <- getPerformances2()
  #2 in [3,3] and 3 in [4, 3] - variant 2 should be on third position, variant 3 should be 4th
  rank.related = matrix(c(2,3,3,
                          3,4,3), ncol=3, byrow=TRUE)
  ranks <- findExtremeRanks(perf = performances, 
                                  strict.vf = TRUE, 
                                  strong.prefs = NULL, 
                                  strong.intensities.of.prefs = NULL, 
                                  rank.related.requirements = rank.related, nums.of.characteristic.points=c()) 
  
  expect_that(ranks[2,1],is_equivalent_to(3))
  expect_that(ranks[2,2],is_equivalent_to(3))
  expect_that(ranks[3,1],is_equivalent_to(4))
  expect_that(ranks[3,2],is_equivalent_to(4))
  #print("ERA rank related preference information with characteristic points OK")
})


test_that("ERA hierarchical structure work properly for extreme rankings - costs", {
  performances <- getPerformancesHierarchicalInverted()
  hierarchy.data <- getHierarchyData()
  strong.preference.intensities = matrix(c("a4","a5", "a4","a3", "nodes1"),ncol=5, byrow=TRUE) #kryterium 1 ważniejsze
  strong.preference = matrix(c("a1","a2", "nodes2"),ncol=3, byrow=TRUE)
  criteria <- c("c", "c", "c", "c");
 
  results <- findExtremeRanksHierarchical(perf = performances, 
                                          strict.vf = TRUE, 
                                          strong.prefs = strong.preference, 
                                          strong.intensities.of.prefs = strong.preference.intensities, 
                                          rank.related.requirements = NULL,nums.of.characteristic.points=c(), criteria=criteria, hierarchy.data = hierarchy.data) 
  
  nodes1.ranks = results$nodes1
  nodes2.ranks = results$nodes2
  nodes.ranks = results$nodes
  expect_that(nodes1.ranks[2,1],is_more_than(nodes1.ranks[1,2]))
  expect_that(nodes2.ranks[6,1],is_equivalent_to(nodes2.ranks[5,1]))
  expect_that(nodes2.ranks[2,2],is_more_than(nodes2.ranks[1,1]))
  indif.preference.intensities = matrix(c("a4","a3", "a4","a5", "nodes1"),ncol=5, byrow=TRUE)
  indif.preference = matrix(c("a1","a2", "nodes2"),ncol=3, byrow=TRUE)
  
  results <- findExtremeRanksHierarchical(perf = performances, 
                                          strict.vf = TRUE, 
                                          indif.prefs = indif.preference, 
                                          indif.intensities.of.prefs = indif.preference.intensities, 
                                          rank.related.requirements = NULL,nums.of.characteristic.points=c(), criteria=criteria, hierarchy.data = hierarchy.data) 
  nodes1.ranks = results$nodes1
  nodes2.ranks = results$nodes2
  
  expect_that(nodes1.ranks[2,2],is_equivalent_to(nodes1.ranks[1,2]))
  expect_that(nodes1.ranks[2,1],is_equivalent_to(nodes1.ranks[1,1]))
  expect_that(nodes2.ranks[1,2],is_equivalent_to(nodes2.ranks[2,2]))
  expect_that(nodes2.ranks[1,1],is_equivalent_to(nodes2.ranks[2,1]))
  
  weak.preference = matrix(c("a1","a2", "nodes2"),ncol=3, byrow=TRUE)
  weak.preference.intensities = matrix(c("a4","a3", "a4","a5", "nodes1"),ncol=5, byrow=TRUE)
  
  results <- findExtremeRanksHierarchical(perf = performances, 
                                          strict.vf = TRUE, 
                                          weak.prefs = weak.preference, 
                                          weak.intensities.of.prefs =  weak.preference.intensities, 
                                          rank.related.requirements = NULL,nums.of.characteristic.points=c(), criteria=criteria, hierarchy.data = hierarchy.data) 
  nodes1.ranks = results$nodes1
  nodes2.ranks = results$nodes2
  expect_that(nodes1.ranks[1,2] + 1,is_more_than(nodes1.ranks[2,1]))  #x+1 > y: x greater or equal y
  expect_that(nodes2.ranks[2,1] + 1,is_more_than(nodes2.ranks[1,1]))  #x+1 > y: x greater or equal y
  
  rank.related <- matrix(c("a2",2,2, "nodes1"),ncol=4, byrow=TRUE)
  results <- findExtremeRanksHierarchical(perf = getPerformancesHierarchical2Inverted(), 
                                          strict.vf = TRUE, 
                                          rank.related.requirements = rank.related,nums.of.characteristic.points=c(), criteria=criteria, hierarchy.data = hierarchy.data) 
  nodes1.ranks = results$nodes1
  nodes2.ranks = results$nodes2
  nodes.ranks = results$nodes
  
  expect_that(nodes1.ranks[2,2],is_equivalent_to(2))  
  expect_that(nodes1.ranks[2,1],is_equivalent_to(2))  
  expect_that(nodes1.ranks[3,2],is_equivalent_to(1))  
  expect_that(nodes1.ranks[3,1],is_equivalent_to(1))  
  
  rank.related <- matrix(c("a2",2,2, "nodes1"),ncol=4, byrow=TRUE)
  results <- findExtremeRanksHierarchical(perf = getPerformancesHierarchical2Inverted(), 
                                          strict.vf = TRUE, 
                                          rank.related.requirements = rank.related,nums.of.characteristic.points=c(2,2,2,2), criteria=criteria, hierarchy.data = hierarchy.data) 
  nodes1.ranks = results$nodes1
  nodes2.ranks = results$nodes2
  nodes.ranks = results$nodes
  
  expect_that(nodes1.ranks[2,2],is_equivalent_to(2))  
  expect_that(nodes1.ranks[2,1],is_equivalent_to(2))  
  expect_that(nodes1.ranks[3,2],is_equivalent_to(1))  
  expect_that(nodes1.ranks[3,1],is_equivalent_to(1))  
  #print("ERA hierarchical structure work properly for extreme rankings OK")
})

test_that("ERA strong preference - two characteristic points per function - cost", {
  performances <- performances <- matrix(c(6,10,9,10,
                                           10,6,10,9,
                                           6,10,6,6,
                                           6,6,6,6,
                                           10,6,1,1,
                                           1,1,1,1), 
                                         ncol=4, 
                                         byrow=TRUE);
  rownames(performances)=c("a1", "a2", "a3", "a4", "a5", "a6");
  colnames(performances)=c("c01", "c02", "c03", "c04");
  #if 5 > 2 then 3 > 2
  strong.preference = matrix(c(5,4), ncol=2,byrow=TRUE)  
  ranks <- findExtremeRanks(perf = performances, 
                            strict.vf = TRUE, 
                            strong.prefs = strong.preference, 
                            strong.intensities.of.prefs = NULL, 
                            rank.related.requirements = NULL,
                            nums.of.characteristic.points=c(2,2,2,2),
                            criteria=c("c", "c", "c", "c")) 
  #print(ranks)
  expect_that(ranks[1,1],is_equivalent_to(6))
  expect_that(ranks[2,1],is_equivalent_to(6))
  expect_that(ranks[3,1],is_equivalent_to(5))
  expect_that(ranks[4,1],is_equivalent_to(3))
  expect_that(ranks[5,1],is_equivalent_to(2))
  expect_that(ranks[6,1],is_equivalent_to(1))
  
  expect_that(ranks[1,2],is_equivalent_to(5))
  expect_that(ranks[2,2],is_equivalent_to(4))
  expect_that(ranks[3,2],is_equivalent_to(4))
  expect_that(ranks[4,2],is_equivalent_to(3))
  expect_that(ranks[5,2],is_equivalent_to(2))
  expect_that(ranks[6,2],is_equivalent_to(1))
})



