
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

test_that("Post Factum possible improvement and detorientation works properly - Hierarchical", {
  hierarchy.data <- getHierarchyData()
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
  strong.preference = matrix(c("a3","a2", 0),ncol=3, byrow=TRUE)
  
  rank.related = matrix(c("a3", 2,2),ncol=3, byrow=TRUE)
  improvement <- findPerformanceModificationForPossibleRelationHierarchical(perf=performances, a=5, b=3, strict.vf=TRUE, strong.prefs=strong.preference,
                                                                  weak.prefs=NULL, indif.prefs = NULL,
                                                                  strong.intensities.of.prefs =NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                  rank.related.requirements = NULL, nums.of.characteristic.points=c(), precision=0.005, 
                                                                  which.attributes = c(0,1,0,0), greater.than.one = TRUE, hierarchy.data=hierarchy.data)
  
  #print(improvement$result)
  strong.preference = matrix(c("a3","a2", 0),ncol=3, byrow=TRUE)
  rank.related = matrix(c("a3", 2,2),ncol=3, byrow=TRUE)
  #print(improvement[["nodes11"]]$result)
  #print(improvement[["nodes12"]]$result)
  #print(improvement[["nodes1"]]$result)
  #print(improvement[["nodes2"]]$result)
  #print(improvement[["nodes"]]$result)
  #print("")
  expect_that(improvement[["nodes"]]$result,is_more_than(2))
  expect_that(improvement[["nodes"]]$result,is_less_than(2.1))
  strong.intensities.of.prefs =NULL# matrix(c("a3","a5", "a4", "a5", 0),ncol=5, byrow=TRUE)
  deterioration <- findPerformanceModificationForPossibleRelationHierarchical(perf=performances, a=3, b=5, strict.vf=TRUE, strong.prefs=NULL,
                                                                    weak.prefs=NULL, indif.prefs = NULL,
                                                                    strong.intensities.of.prefs=strong.intensities.of.prefs, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                    rank.related.requirements = NULL, nums.of.characteristic.points=c(), precision=0.005, 
                                                                    which.attributes = c(0,1,0,0), greater.than.one = FALSE, hierarchy.data=hierarchy.data)
  #print(deterioration$result)
  expect_that(deterioration[["nodes"]]$result,is_more_than(0.5))
  expect_that(deterioration[["nodes"]]$result,is_less_than(0.6))
  #print(deterioration[["nodes11"]]$result)
  #print(deterioration[["nodes12"]]$result)
  #print(deterioration[["nodes1"]]$result)
  #print(deterioration[["nodes2"]]$result)
  #print(deterioration[["nodes"]]$result)
  #print("")
  improvement <- findPerformanceModificationForPossibleRelationHierarchical(perf=performances, a=5, b=4, strict.vf=TRUE, strong.prefs=strong.preference,
                                                                  weak.prefs=NULL, indif.prefs = NULL,
                                                                  strong.intensities.of.prefs =NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                  rank.related.requirements = NULL, nums.of.characteristic.points=c(), precision=0.005, 
                                                                  which.attributes = c(1,0,0,0), greater.than.one = TRUE, hierarchy.data=hierarchy.data)
  #print(improvement$result)
  
  expect_that(improvement[["nodes"]]$result,is_more_than(5))
  expect_that(improvement[["nodes"]]$result,is_less_than(5.1))
  #print(improvement[["nodes11"]]$result)
  #print(improvement[["nodes12"]]$result)
  #print(improvement[["nodes1"]]$result)
  #print(improvement[["nodes2"]]$result)
  #print(improvement[["nodes"]]$result)
  #print("")
  #print("Post factum possible improvement works properly")
})


test_that("Post Factum necessary improvement and detorientation works properly - Hierarchical", {
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
  strong.preference = NULL##NULLmatrix(c("a3","a2", 0),ncol=3, byrow=TRUE)
  hierarchy.data <- getHierarchyData()
  rank.related = NULL#matrix(c("a3", 3,3),ncol=3, byrow=TRUE)
  improvement <- findPerformanceModificationForNecessaryRelationHierarchical(perf=performances, a=5, b=4, strict.vf=TRUE, strong.prefs=strong.preference,
                                                                   weak.prefs=NULL, indif.prefs = NULL,
                                                                   strong.intensities.of.prefs =NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                   rank.related.requirements = rank.related, nums.of.characteristic.points=c(), precision=0.005, 
                                                                   which.attributes = c(1,1,1,1), greater.than.one = TRUE, hierarchy.data=hierarchy.data)
  
  rownames(performances)=c("a1", "a2", "a3", "a4", "a5", "a6");
  colnames(performances)=c("c01", "c02", "c03", "c04");
  strong.preference = matrix(c("a3","a2", 0),ncol=3, byrow=TRUE)
  #print(improvement[["nodes11"]]$result)
  #print(improvement[["nodes12"]]$result)
  #print(improvement[["nodes1"]]$result)
  #print(improvement[["nodes2"]]$result)
  #print(improvement[["nodes"]]$result)
  #rank.related = matrix(c("a3", 2,2),ncol=3, byrow=TRUE)
  expect_that(improvement[["nodes"]]$result,is_more_than(5))
  expect_that(improvement[["nodes"]]$result,is_less_than(5.1))
  
  deterioration <- findPerformanceModificationForNecessaryRelationHierarchical(perf=performances, a=4, b=5, strict.vf=TRUE, strong.prefs=strong.preference,
                                                                     weak.prefs=NULL, indif.prefs = NULL,
                                                                     strong.intensities.of.prefs =NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                     rank.related.requirements = rank.related, nums.of.characteristic.points=c(), precision=0.005, 
                                                                     which.attributes = c(1,1,1,1), greater.than.one = FALSE, hierarchy.data=hierarchy.data)
  
  #print(deterioration[["nodes11"]]$result)
  #print(deterioration[["nodes12"]]$result)
  #print(deterioration[["nodes1"]]$result)
  #print(deterioration[["nodes2"]]$result)
  #print(deterioration[["nodes"]]$result)
  
  expect_that(deterioration[["nodes"]]$result,is_more_than(1.0))
  expect_that(deterioration[["nodes"]]$result,is_less_than(1.01))
  deterioration <- findPerformanceModificationForNecessaryRelationHierarchical(perf=performances, a=6, b=1, strict.vf=TRUE, strong.prefs=strong.preference,
                                                                     weak.prefs=NULL, indif.prefs = NULL,
                                                                     strong.intensities.of.prefs =NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                     rank.related.requirements = rank.related, nums.of.characteristic.points=c(), precision=0.005, 
                                                                     which.attributes = c(1,1,1,1), greater.than.one = FALSE, hierarchy.data=hierarchy.data)
  
  #print(deterioration[["nodes11"]]$result)
  #print(deterioration[["nodes12"]]$result)
  #print(deterioration[["nodes1"]]$result)
  #print(deterioration[["nodes2"]]$result)
  #print(deterioration[["nodes"]]$result)
  expect_that(deterioration[["nodes"]]$result,is_more_than(0.1))
  expect_that(deterioration[["nodes"]]$result,is_less_than(0.11))
  #print("Post factum necessary improvement works properly")
})

test_that("Post Factum finding missing Utility works properly for possible relations. - Hierarchical version", {
  #POSSIBLE
  
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
  strong.preference = NULL#matrix(c("a3","a2", 0),ncol=3, byrow=TRUE)
  hierarchy.data <- getHierarchyData()
  rank.related = matrix(c("a3", 2,2),ncol=3, byrow=TRUE)
  
  umissing1 <- findMissingOrRedundantUtilityValueForRelationHierarchical(perf = performances, a = 5, b = 6, strict.vf = TRUE, is.possibly.preffered = TRUE,
                                                              strong.prefs=strong.preference, weak.prefs=NULL, indif.prefs = NULL, 
                                                              strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                              rank.related.requirements = rank.related, nums.of.characteristic.points=c(2,2,2,2), improvement=TRUE, hierarchy.data=hierarchy.data)
  umissing2 <- findMissingOrRedundantUtilityValueForRelationHierarchical(perf = performances, a = 3, b = 6, strict.vf = TRUE, is.possibly.preffered = TRUE,
                                                              strong.prefs=strong.preference, weak.prefs=NULL, indif.prefs = NULL, 
                                                              strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                              rank.related.requirements = rank.related, nums.of.characteristic.points=c(2,2,2,2), improvement=TRUE, hierarchy.data=hierarchy.data)
  expect_that(umissing1[["nodes"]]$result,is_more_than(umissing2[["nodes"]]$result))
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
  strong.int.of.preference = matrix(c("a4","a5", "a3", "a5", 0),ncol=5, byrow=TRUE) # criterion c1 is more important
  
  umissing1 <- findMissingOrRedundantUtilityValueForRelationHierarchical(perf = performances, a = 3, b = 5, strict.vf = TRUE, is.possibly.preffered = TRUE,
                                                  strong.prefs=NULL, weak.prefs=NULL, indif.prefs = NULL, 
                                                  strong.intensities.of.prefs =strong.int.of.preference , weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                  rank.related.requirements = NULL, nums.of.characteristic.points=c(2,2,2,2), improvement=FALSE, hierarchy.data=hierarchy.data)
  umissing2 <- findMissingOrRedundantUtilityValueForRelationHierarchical(perf = performances, a = 4, b = 5, strict.vf = TRUE, is.possibly.preffered = TRUE,
                                                  strong.prefs=NULL, weak.prefs=NULL, indif.prefs = NULL, 
                                                  strong.intensities.of.prefs =strong.int.of.preference , weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                  rank.related.requirements = NULL, nums.of.characteristic.points=c(2,2,2,2), improvement=FALSE, hierarchy.data=hierarchy.data)
  expect_that(umissing2[["nodes"]]$result,is_more_than(umissing1[["nodes"]]$result))
  
  
  #NECESSARY
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
  strong.preference = NULL#matrix(c("a3","a2", 0),ncol=3, byrow=TRUE)
  
  rank.related = matrix(c("a3", 2,2),ncol=3, byrow=TRUE)
  umissing1 <- findMissingOrRedundantUtilityValueForRelationHierarchical(perf = performances, a = 5, b = 6, strict.vf = TRUE, is.possibly.preffered = FALSE,
                                                  strong.prefs=strong.preference, weak.prefs=NULL, indif.prefs = NULL, 
                                                  strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                  rank.related.requirements = rank.related, nums.of.characteristic.points=c(2,2,2,2), improvement=TRUE, hierarchy.data=hierarchy.data)
  umissing2 <- findMissingOrRedundantUtilityValueForRelationHierarchical(perf = performances, a = 3, b = 6, strict.vf = TRUE, is.possibly.preffered = FALSE,
                                                  strong.prefs=strong.preference, weak.prefs=NULL, indif.prefs = NULL, 
                                                  strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                  rank.related.requirements = rank.related, nums.of.characteristic.points=c(2,2,2,2), improvement=TRUE, hierarchy.data=hierarchy.data)
  
  expect_that(umissing1[["nodes"]]$result,is_more_than(umissing2[["nodes"]]$result))
  
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
  strong.int.of.preference = matrix(c("a4","a5", "a3", "a5", "nodes1"),ncol=5, byrow=TRUE) # criterion c1 is more important
  
  umissing1 <- findMissingOrRedundantUtilityValueForRelationHierarchical(perf = performances, a = 3, b = 5, strict.vf = TRUE, is.possibly.preffered = FALSE,
                                                  strong.prefs=NULL, weak.prefs=NULL, indif.prefs = NULL, 
                                                  strong.intensities.of.prefs =strong.int.of.preference , weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                  rank.related.requirements = NULL, nums.of.characteristic.points=c(2,2,2,2), improvement=FALSE, hierarchy.data=hierarchy.data)
  umissing2 <- findMissingOrRedundantUtilityValueForRelationHierarchical(perf = performances, a = 4, b = 5, strict.vf = TRUE, is.possibly.preffered = FALSE,
                                                  strong.prefs=NULL, weak.prefs=NULL, indif.prefs = NULL, 
                                                  strong.intensities.of.prefs =strong.int.of.preference , weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                  rank.related.requirements = NULL, nums.of.characteristic.points=c(2,2,2,2), improvement=FALSE, hierarchy.data=hierarchy.data)
  expect_that(umissing2[["nodes"]]$result,is_more_than(umissing1[["nodes"]]$result))
  expect_that(umissing2[["nodes1"]]$result,is_more_than(umissing1[["nodes1"]]$result))
})


test_that("Post Factum finding missing Utility works properly for possible relations.", {
  #POSSIBLE
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
  strong.preference = NULL#matrix(c("a3","a2", 0),ncol=3, byrow=TRUE)
  
  rank.related = matrix(c("a3", 2,2),ncol=3, byrow=TRUE)
  umissing1 <- findMissingOrRedundantUtilityValueForRelation(perf = performances, a = 5, b = 6, strict.vf = TRUE, is.possibly.preffered = TRUE,
                                                                        strong.prefs=strong.preference, weak.prefs=NULL, indif.prefs = NULL, 
                                                                        strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                        rank.related.requirements = rank.related, nums.of.characteristic.points=c(2,2,2,2), improvement=TRUE)
  umissing2 <- findMissingOrRedundantUtilityValueForRelation(perf = performances, a = 3, b = 6, strict.vf = TRUE, is.possibly.preffered = TRUE,
                                                                        strong.prefs=strong.preference, weak.prefs=NULL, indif.prefs = NULL, 
                                                                        strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                        rank.related.requirements = rank.related, nums.of.characteristic.points=c(2,2,2,2), improvement=TRUE)
  expect_that(umissing1$result,is_more_than(umissing2$result))
  
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
  strong.int.of.preference = matrix(c("a4","a5", "a3", "a5", 0),ncol=5, byrow=TRUE) # criterion c1 is more important
  
  umissing1 <- findMissingOrRedundantUtilityValueForRelation(perf = performances, a = 3, b = 5, strict.vf = TRUE, is.possibly.preffered = TRUE,
                                                                         strong.prefs=NULL, weak.prefs=NULL, indif.prefs = NULL, 
                                                                         strong.intensities.of.prefs =strong.int.of.preference , weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                         rank.related.requirements = NULL, nums.of.characteristic.points=c(2,2,2,2), improvement=FALSE)
  umissing2 <- findMissingOrRedundantUtilityValueForRelation(perf = performances, a = 4, b = 5, strict.vf = TRUE, is.possibly.preffered = TRUE,
                                                                         strong.prefs=NULL, weak.prefs=NULL, indif.prefs = NULL, 
                                                                         strong.intensities.of.prefs =strong.int.of.preference , weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                         rank.related.requirements = NULL, nums.of.characteristic.points=c(2,2,2,2), improvement=FALSE)
  expect_that(umissing2$result,is_more_than(umissing1$result))
  
  
  #NECESSARY
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
  strong.preference = NULL#matrix(c("a3","a2", 0),ncol=3, byrow=TRUE)
  
  rank.related = matrix(c("a3", 2,2),ncol=3, byrow=TRUE)
  umissing1 <- findMissingOrRedundantUtilityValueForRelation(perf = performances, a = 5, b = 6, strict.vf = TRUE, is.possibly.preffered = FALSE,
                                                                         strong.prefs=strong.preference, weak.prefs=NULL, indif.prefs = NULL, 
                                                                         strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                         rank.related.requirements = rank.related, nums.of.characteristic.points=c(2,2,2,2), improvement=TRUE)
  umissing2 <- findMissingOrRedundantUtilityValueForRelation(perf = performances, a = 3, b = 6, strict.vf = TRUE, is.possibly.preffered = FALSE,
                                                                         strong.prefs=strong.preference, weak.prefs=NULL, indif.prefs = NULL, 
                                                                         strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                         rank.related.requirements = rank.related, nums.of.characteristic.points=c(2,2,2,2), improvement=TRUE)
  
  expect_that(umissing1$result,is_more_than(umissing2$result))
  
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
  strong.int.of.preference = matrix(c("a4","a5", "a3", "a5", 0),ncol=5, byrow=TRUE) # criterion c1 is more important
  
  umissing1 <- findMissingOrRedundantUtilityValueForRelation(perf = performances, a = 3, b = 5, strict.vf = TRUE, is.possibly.preffered = FALSE,
                                                                         strong.prefs=NULL, weak.prefs=NULL, indif.prefs = NULL, 
                                                                         strong.intensities.of.prefs =strong.int.of.preference , weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                         rank.related.requirements = NULL, nums.of.characteristic.points=c(2,2,2,2), improvement=FALSE)
  umissing2 <- findMissingOrRedundantUtilityValueForRelation(perf = performances, a = 4, b = 5, strict.vf = TRUE, is.possibly.preffered = FALSE,
                                                                         strong.prefs=NULL, weak.prefs=NULL, indif.prefs = NULL, 
                                                                         strong.intensities.of.prefs =strong.int.of.preference , weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                         rank.related.requirements = NULL, nums.of.characteristic.points=c(2,2,2,2), improvement=FALSE)
  expect_that(umissing2$result,is_more_than(umissing1$result))
  
})
test_that("Post Factum possible improvement and detorientation works properly.", {
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
  strong.preference = matrix(c("a3","a2", 0),ncol=3, byrow=TRUE)
  
  rank.related = matrix(c("a3", 2,2),ncol=3, byrow=TRUE)
  improvement <- findPerformanceModificationForPossibleRelation(perf=performances, a=5, b=3, strict.vf=TRUE, strong.prefs=strong.preference,
                                                  weak.prefs=NULL, indif.prefs = NULL,
                                                  strong.intensities.of.prefs =NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                  rank.related.requirements = NULL, nums.of.characteristic.points=c(), precision=0.005, 
                                                  which.attributes = c(0,1,0,0), greater.than.one = TRUE)
  
  #print(improvement$result)
  rownames(performances)=c("a1", "a2", "a3", "a4", "a5", "a6");
  colnames(performances)=c("g1", "g2", "g3", "g4");
  strong.preference = matrix(c("a3","a2", 0),ncol=3, byrow=TRUE)
  rank.related = matrix(c("a3", 2,2),ncol=3, byrow=TRUE)
  expect_that(improvement$result,is_more_than(2))
  expect_that(improvement$result,is_less_than(2.1))
  strong.intensities.of.prefs =NULL# matrix(c("a3","a5", "a4", "a5", 0),ncol=5, byrow=TRUE)
  deterioration <- findPerformanceModificationForPossibleRelation(perf=performances, a=3, b=5, strict.vf=TRUE, strong.prefs=NULL,
                                                  weak.prefs=NULL, indif.prefs = NULL,
                                                  strong.intensities.of.prefs=strong.intensities.of.prefs, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                  rank.related.requirements = NULL, nums.of.characteristic.points=c(), precision=0.005, 
                                                  which.attributes = c(0,1,0,0), greater.than.one = FALSE)
  #print(deterioration$result)
  expect_that(deterioration$result,is_more_than(0.5))
  expect_that(deterioration$result,is_less_than(0.6))
  
  improvement <- findPerformanceModificationForPossibleRelation(perf=performances, a=5, b=4, strict.vf=TRUE, strong.prefs=strong.preference,
                                                  weak.prefs=NULL, indif.prefs = NULL,
                                                  strong.intensities.of.prefs =NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                  rank.related.requirements = NULL, nums.of.characteristic.points=c(), precision=0.005, 
                                                  which.attributes = c(1,0,0,0), greater.than.one = TRUE)
    #print(improvement$result)
  
  expect_that(improvement$result,is_more_than(5))
  expect_that(improvement$result,is_less_than(5.1))
  #print("Post factum possible improvement works properly")
})


test_that("Post Factum necessary improvement and detorientation works properly.", {
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
  strong.preference = matrix(c("a3","a2", 0),ncol=3, byrow=TRUE)
  rank.related = NULL#matrix(c("a3", 3,3),ncol=3, byrow=TRUE)
  #ranks <- findExtremeRanks(perf=performances, 
  #                 strict.vf=TRUE, 
  #                 strong.prefs=strong.preference,
  #                 weak.prefs=NULL, indif.prefs = NULL,
  #                 strong.intensities.of.prefs =NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
  #                 rank.related.requirements = rank.related, nums.of.characteristic.points=c())
  improvement <- findPerformanceModificationForNecessaryRelation(perf=performances, a=5, b=4, strict.vf=TRUE, strong.prefs=strong.preference,
                                                  weak.prefs=NULL, indif.prefs = NULL,
                                                  strong.intensities.of.prefs =NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                  rank.related.requirements = rank.related, nums.of.characteristic.points=c(), precision=0.005, 
                                                  which.attributes = c(1,1,1,1), greater.than.one = TRUE)
  #print(improvement$result)
  rownames(performances)=c("a1", "a2", "a3", "a4", "a5", "a6");
  colnames(performances)=c("g1", "g2", "g3", "g4");
  strong.preference = matrix(c("a3","a2", 0),ncol=3, byrow=TRUE)
  #rank.related = matrix(c("a3", 2,2),ncol=3, byrow=TRUE)
  expect_that(improvement$result,is_more_than(5))
  expect_that(improvement$result,is_less_than(5.1))
  #print(improvement$result)
  deterioration <- findPerformanceModificationForNecessaryRelation(perf=performances, a=4, b=5, strict.vf=TRUE, strong.prefs=strong.preference,
                                                   weak.prefs=NULL, indif.prefs = NULL,
                                                   strong.intensities.of.prefs =NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                   rank.related.requirements = rank.related, nums.of.characteristic.points=c(), precision=0.005, 
                                                   which.attributes = c(1,1,1,1), greater.than.one = FALSE)
  
  #print(deterioration$result)
  expect_that(deterioration$result,is_more_than(1.0))
  expect_that(deterioration$result,is_less_than(1.01))
  deterioration <- findPerformanceModificationForNecessaryRelation(perf=performances, a=6, b=1, strict.vf=TRUE, strong.prefs=strong.preference,
                                                     weak.prefs=NULL, indif.prefs = NULL,
                                                     strong.intensities.of.prefs =NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                     rank.related.requirements = rank.related, nums.of.characteristic.points=c(), precision=0.005, 
                                                     which.attributes = c(1,1,1,1), greater.than.one = FALSE)
  
  #print(deterioration$result)
  expect_that(deterioration$result,is_more_than(0.1))
  expect_that(deterioration$result,is_less_than(0.11))
  #print("Post factum necessary improvement works properly")
})


