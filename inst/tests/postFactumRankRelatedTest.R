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

test_that("Post Factum finding missing Utility works properly for ranks - hierarchical", {
  hierarchy.data=getHierarchyData()
  #POSSIBLE
  #improvement
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
  strong.preference = NULL
  
  rank.related = matrix(c("a3", 2,2),ncol=3, byrow=TRUE)
  umissing1 <- findMissingOrRedundantUtilityValueForRankPositionHierarchical(perf=performances, a=1, k=2, strict.vf=TRUE, is.possibly.preffered=TRUE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = rank.related,
                                                                 nums.of.characteristic.points=NULL,
                                                                 improvement=TRUE, hierarchy.data=hierarchy.data)
  umissing2 <- findMissingOrRedundantUtilityValueForRankPositionHierarchical(perf=performances, a=3, k=2, strict.vf=TRUE, is.possibly.preffered=TRUE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = rank.related,
                                                                 nums.of.characteristic.points=NULL,
                                                                 improvement=TRUE, hierarchy.data=hierarchy.data)
  
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
  strong.preference = NULL
  
  umissing1 <- findMissingOrRedundantUtilityValueForRankPositionHierarchical(perf=performances, a=5, k=1, strict.vf=TRUE, is.possibly.preffered=TRUE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,
                                                                 nums.of.characteristic.points=NULL,
                                                                 improvement=TRUE, hierarchy.data=hierarchy.data)
  umissing2 <- findMissingOrRedundantUtilityValueForRankPositionHierarchical(perf=performances, a=4, k=1, strict.vf=TRUE, is.possibly.preffered=TRUE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = rank.related,
                                                                 nums.of.characteristic.points=NULL,
                                                                 improvement=TRUE, hierarchy.data=hierarchy.data)
  expect_that(umissing1[["nodes"]]$result,is_more_than(umissing2[["nodes"]]$result))
  #deterioration
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
  strong.preference = NULL
  
  umissing1 <- findMissingOrRedundantUtilityValueForRankPositionHierarchical(perf=performances, a=6, k=2, strict.vf=TRUE, is.possibly.preffered=TRUE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,
                                                                 nums.of.characteristic.points=NULL,
                                                                 improvement=FALSE, hierarchy.data=hierarchy.data)
  umissing2 <- findMissingOrRedundantUtilityValueForRankPositionHierarchical(perf=performances, a=3, k=2, strict.vf=TRUE, is.possibly.preffered=TRUE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,
                                                                 nums.of.characteristic.points=NULL,
                                                                 improvement=FALSE, hierarchy.data=hierarchy.data)
  expect_that(umissing1[["nodes"]]$result,is_more_than(umissing2[["nodes"]]$result))
  
  #NECESSARY 
  #IMPROVEMENT
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
  strong.preference = NULL
  
  umissing1 <- findMissingOrRedundantUtilityValueForRankPositionHierarchical(perf=performances, a=1, k=3, strict.vf=TRUE, is.possibly.preffered=FALSE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,
                                                                 nums.of.characteristic.points=c(2,2,2,2),
                                                                 improvement=TRUE, hierarchy.data=hierarchy.data)
  umissing2 <- findMissingOrRedundantUtilityValueForRankPositionHierarchical(perf=performances, a=2, k=3, strict.vf=TRUE, is.possibly.preffered=FALSE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,
                                                                 nums.of.characteristic.points=c(2,2,2,2),
                                                                 improvement=TRUE, hierarchy.data=hierarchy.data)
  
  expect_that(umissing1[["nodes"]]$result,is_more_than(umissing2[["nodes"]]$result))
  #DETERIORATION
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
  strong.preference = NULL  
  
  umissing1 <- findMissingOrRedundantUtilityValueForRankPositionHierarchical(perf=performances, a=6, k=3, strict.vf=TRUE, is.possibly.preffered=FALSE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,
                                                                 nums.of.characteristic.points=c(2,2,2,2),
                                                                 improvement=FALSE, hierarchy.data=hierarchy.data)
  umissing2 <- findMissingOrRedundantUtilityValueForRankPositionHierarchical(perf=performances, a=3, k=3, strict.vf=TRUE, is.possibly.preffered=FALSE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,
                                                                 nums.of.characteristic.points=c(2,2,2,2),
                                                                 improvement=FALSE, hierarchy.data=hierarchy.data)
  
  expect_that(umissing1[["nodes"]]$result,is_more_than(umissing2[["nodes"]]$result))
})

test_that("Post Factum finding missing Utility works properly for ranks.", {
  #POSSIBLE
    #improvement
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
  strong.preference = NULL
  
  rank.related = matrix(c("a3", 2,2),ncol=3, byrow=TRUE)
  umissing1 <- findMissingOrRedundantUtilityValueForRankPosition(perf=performances, a=1, k=2, strict.vf=TRUE, is.possibly.preffered=TRUE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = rank.related,
                                                                 nums.of.characteristic.points=NULL,
                                                                 improvement=TRUE)
  umissing2 <- findMissingOrRedundantUtilityValueForRankPosition(perf=performances, a=3, k=2, strict.vf=TRUE, is.possibly.preffered=TRUE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = rank.related,
                                                                 nums.of.characteristic.points=NULL,
                                                                 improvement=TRUE)
  
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
  colnames(performances)=c("c01", "c02", "c03", "c04");
  strong.preference = NULL
  
  umissing1 <- findMissingOrRedundantUtilityValueForRankPosition(perf=performances, a=5, k=1, strict.vf=TRUE, is.possibly.preffered=TRUE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,
                                                                 nums.of.characteristic.points=NULL,
                                                                 improvement=TRUE)
  umissing2 <- findMissingOrRedundantUtilityValueForRankPosition(perf=performances, a=4, k=1, strict.vf=TRUE, is.possibly.preffered=TRUE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = rank.related,
                                                                 nums.of.characteristic.points=NULL,
                                                                 improvement=TRUE)
  expect_that(umissing1$result,is_more_than(umissing2$result))
  #deterioration
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
  strong.preference = NULL
  
  umissing1 <- findMissingOrRedundantUtilityValueForRankPosition(perf=performances, a=6, k=2, strict.vf=TRUE, is.possibly.preffered=TRUE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,
                                                                 nums.of.characteristic.points=NULL,
                                                                 improvement=FALSE)
  umissing2 <- findMissingOrRedundantUtilityValueForRankPosition(perf=performances, a=3, k=2, strict.vf=TRUE, is.possibly.preffered=TRUE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,
                                                                 nums.of.characteristic.points=NULL,
                                                                 improvement=FALSE)
  expect_that(umissing1$result,is_more_than(umissing2$result))

  #NECESSARY 
    #IMPROVEMENT
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
  strong.preference = NULL
  
  umissing1 <- findMissingOrRedundantUtilityValueForRankPosition(perf=performances, a=1, k=3, strict.vf=TRUE, is.possibly.preffered=FALSE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,
                                                                 nums.of.characteristic.points=c(2,2,2,2),
                                                                 improvement=TRUE)
  umissing2 <- findMissingOrRedundantUtilityValueForRankPosition(perf=performances, a=2, k=3, strict.vf=TRUE, is.possibly.preffered=FALSE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,
                                                                 nums.of.characteristic.points=c(2,2,2,2),
                                                                 improvement=TRUE)
  
  expect_that(umissing1$result,is_more_than(umissing2$result))
  #DETERIORATION
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
  strong.preference = NULL  
  
  umissing1 <- findMissingOrRedundantUtilityValueForRankPosition(perf=performances, a=6, k=3, strict.vf=TRUE, is.possibly.preffered=FALSE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,
                                                                 nums.of.characteristic.points=c(2,2,2,2),
                                                                 improvement=FALSE)
  umissing2 <- findMissingOrRedundantUtilityValueForRankPosition(perf=performances, a=3, k=3, strict.vf=TRUE, is.possibly.preffered=FALSE,
                                                                 strong.prefs=NULL,weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL,
                                                                 weak.intensities.of.prefs = NULL,
                                                                 indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,
                                                                 nums.of.characteristic.points=c(2,2,2,2),
                                                                 improvement=FALSE)

  expect_that(umissing1$result,is_more_than(umissing2$result))
})

test_that("Post Factum performance improvement and deterioration for rank target work properly", {
  
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
  
  result <- findPossibleRankRelatedPerformanceModification(perf=performances, a=1, k=3, strict.vf=TRUE, strong.prefs=NULL,
                                                                 weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                                 which.attributes = c(1,1,1,1), greater.than.one = TRUE)
   
  expect_that(result$result,is_more_than(2.005))
  expect_that(result$result,is_less_than(2.01))
  
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
  rank.related = matrix(c("a3", 4,2),ncol=3, byrow=TRUE)
  result <- findPossibleRankRelatedPerformanceModification(perf=performances, a=1, k=1, strict.vf=TRUE, strong.prefs=NULL,
                                                                 weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = rank.related,  nums.of.characteristic.points=NULL, precision=0.005,
                                                                 which.attributes = c(1,1,1,1), greater.than.one = TRUE)
  expect_that(result$result,is_more_than(10))
  expect_that(result$result,is_less_than(10.1))
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
  rank.related = matrix(c("a3", 3,2),ncol=3, byrow=TRUE)
  result <- findPossibleRankRelatedPerformanceModification(perf=performances, a=2, k=2, strict.vf=TRUE, strong.prefs=NULL,
                                                           weak.prefs=NULL, indif.prefs = NULL,
                                                           strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                           rank.related.requirements = rank.related,  nums.of.characteristic.points=NULL, precision=0.005,
                                                           which.attributes = c(1,1,1,1), greater.than.one = TRUE)
  
  expect_that(result$result,is_more_than(0.995))
  expect_that(result$result,is_less_than(1.005))
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
  result <- findPossibleRankRelatedPerformanceModification(perf=performances, a=6, k=1, strict.vf=TRUE, strong.prefs=NULL,
                                                           weak.prefs=NULL, indif.prefs = NULL,
                                                           strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                           rank.related.requirements = FALSE,  nums.of.characteristic.points=NULL, precision=0.005,
                                                           which.attributes = c(1,1,1,1), greater.than.one = FALSE)
  
  expect_that(result$result,is_more_than(0.500))
  expect_that(result$result,is_less_than(0.510))
  
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
  result <- findPossibleRankRelatedPerformanceModification(perf=performances, a=6, k=6, strict.vf=TRUE, strong.prefs=NULL,
                                                           weak.prefs=NULL, indif.prefs = NULL,
                                                           strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                           rank.related.requirements = FALSE,  nums.of.characteristic.points=NULL, precision=0.005,
                                                           which.attributes = c(1,1,1,1), greater.than.one = FALSE)
  
  expect_that(result$result,is_more_than(0.095))
  expect_that(result$result,is_less_than(0.106))

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
  
  improvement <- findNecessaryRankRelatedPerformanceModification(perf=performances, a=1, k=1, strict.vf=TRUE, strong.prefs=NULL,
                                                                 weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                                 which.attributes = c(1,1,1,1), greater.than.one = TRUE)
  expect_that(improvement$result,is_more_than(10))
  expect_that(improvement$result,is_less_than(10.1))
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

  result <- findNecessaryRankRelatedPerformanceModification(perf=performances, a=5, k=2, strict.vf=TRUE, strong.prefs=NULL,
                                                                 weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                                 which.attributes = c(1,1,1,1), greater.than.one = TRUE)
  
  expect_that(result$result,is_more_than(5))
  expect_that(result$result,is_less_than(5.01))
  improvement <- findNecessaryRankRelatedPerformanceModification(perf=performances, a=6, k=1, strict.vf=TRUE, strong.prefs=NULL,
                                                                 weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                                 which.attributes = c(1,1,1,1), greater.than.one = FALSE)
  
  expect_that(improvement$result,is_more_than(1))
  expect_that(improvement$result,is_less_than(1.01))
  
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
  improvement <- findNecessaryRankRelatedPerformanceModification(perf=performances, a=6, k=6, strict.vf=TRUE, strong.prefs=NULL,
                                                                 weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                                 which.attributes = c(1,1,1,1), greater.than.one = FALSE)
 
  expect_that(improvement$result,is_more_than(0.095))
  expect_that(improvement$result,is_less_than(0.105))
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
  improvement <- findNecessaryRankRelatedPerformanceModification(perf=performances, a=6, k=2, strict.vf=TRUE, strong.prefs=NULL,
                                                                 weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                                 which.attributes = c(1,1,1,1), greater.than.one = FALSE)
})

test_that("Post Factum performance improvement for rank target work properly - Hierarchical version", {
  hierarchy.data = getHierarchyData()
  
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
  
  result <- findPossibleRankRelatedPerformanceModificationHierarchical(perf=performances, a=1, k=3, strict.vf=TRUE, strong.prefs=NULL,
                                                           weak.prefs=NULL, indif.prefs = NULL,
                                                           strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                           rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                           which.attributes = c(1,1,1,1), greater.than.one = TRUE, hierarchy.data=hierarchy.data)
  #print(result[["nodes11"]]$result)
  #print(result[["nodes12"]]$result)
  #print(result[["nodes1"]]$result)
  #print(result[["nodes2"]]$result)
  #print(result[["nodes"]]$result)
  #print("- - - - - - - - - - - - - -")
  expect_that(result[["nodes"]]$result,is_more_than(2.005))
  expect_that(result[["nodes"]]$result,is_less_than(2.01))
  
  
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
  rank.related = matrix(c("a3", 4,2),ncol=3, byrow=TRUE)
  result <- findPossibleRankRelatedPerformanceModificationHierarchical(perf=performances, a=1, k=1, strict.vf=TRUE, strong.prefs=NULL,
                                                           weak.prefs=NULL, indif.prefs = NULL,
                                                           strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                           rank.related.requirements = rank.related,  nums.of.characteristic.points=NULL, precision=0.005,
                                                           which.attributes = c(1,1,1,1), greater.than.one = TRUE, hierarchy.data=hierarchy.data)
  
  #print(result[["nodes11"]]$result)
  #print(result[["nodes12"]]$result)
  #print(result[["nodes1"]]$result)
  #print(result[["nodes2"]]$result)
  #print(result[["nodes"]]$result)
  #print("- - - - - - - - - - - - - -")
  expect_that(result[["nodes"]]$result,is_more_than(10))
  expect_that(result[["nodes"]]$result,is_less_than(10.1))
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
  rank.related = matrix(c("a3", 3,2),ncol=3, byrow=TRUE)
  result <- findPossibleRankRelatedPerformanceModificationHierarchical(perf=performances, a=2, k=2, strict.vf=TRUE, strong.prefs=NULL,
                                                           weak.prefs=NULL, indif.prefs = NULL,
                                                           strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                           rank.related.requirements = rank.related,  nums.of.characteristic.points=NULL, precision=0.005,
                                                           which.attributes = c(1,1,1,1), greater.than.one = TRUE, hierarchy.data=hierarchy.data)
  #print(result[["nodes11"]]$result)
  #print(result[["nodes12"]]$result)
  #print(result[["nodes1"]]$result)
  #print(result[["nodes2"]]$result)
  #print(result[["nodes"]]$result)
  #print("- - - - - - - - - - - - - -")
  expect_that(result[["nodes"]]$result,is_more_than(0.995))
  expect_that(result[["nodes"]]$result,is_less_than(1.005))
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
  result <- findPossibleRankRelatedPerformanceModificationHierarchical(perf=performances, a=6, k=1, strict.vf=TRUE, strong.prefs=NULL,
                                                           weak.prefs=NULL, indif.prefs = NULL,
                                                           strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                           rank.related.requirements = FALSE,  nums.of.characteristic.points=NULL, precision=0.005,
                                                           which.attributes = c(1,1,1,1), greater.than.one = FALSE, hierarchy.data=hierarchy.data)
  #print(result[["nodes11"]]$result)
  #print(result[["nodes12"]]$result)
  #print(result[["nodes1"]]$result)
  #print(result[["nodes2"]]$result)
  #print(result[["nodes"]]$result)
  #print("- - - - - - - - - - - - - -")
  expect_that(result[["nodes"]]$result,is_more_than(0.500))
  expect_that(result[["nodes"]]$result,is_less_than(0.510))
  
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
  result <- findPossibleRankRelatedPerformanceModificationHierarchical(perf=performances, a=6, k=6, strict.vf=TRUE, strong.prefs=NULL,
                                                           weak.prefs=NULL, indif.prefs = NULL,
                                                           strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                           rank.related.requirements = FALSE,  nums.of.characteristic.points=NULL, precision=0.005,
                                                           which.attributes = c(1,1,1,1), greater.than.one = FALSE, hierarchy.data=hierarchy.data)
  #print(result[["nodes11"]]$result)
  #print(result[["nodes12"]]$result)
  #print(result[["nodes1"]]$result)
  #print(result[["nodes2"]]$result)
  #print(result[["nodes"]]$result)
  #print("- - - - - - - - - - - - - -")
  
  expect_that(result[["nodes"]]$result,is_more_than(0.095))
  expect_that(result[["nodes"]]$result,is_less_than(0.106))
  

  
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
  
  #rank.related = matrix(c("a3", 2,2),ncol=3, byrow=TRUE)
  improvement <- findNecessaryRankRelatedPerformanceModificationHierarchical(perf=performances, a=1, k=1, strict.vf=TRUE, strong.prefs=NULL,
                                                                 weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                                 which.attributes = c(1,1,1,1), greater.than.one = TRUE, hierarchy.data = hierarchy.data)
  #print(improvement[["nodes11"]]$result)
  #print(improvement[["nodes12"]]$result)
  #print(improvement[["nodes1"]]$result)
  #print(improvement[["nodes2"]]$result)
  #print(improvement[["nodes"]]$result)
  #print("- - - - - - - - - - - - - -")
  expect_that(improvement[["nodes"]]$result,is_more_than(10))
  expect_that(improvement[["nodes"]]$result,is_less_than(10.1))
  improvement <- findNecessaryRankRelatedPerformanceModificationHierarchical(perf=performances, a=1, k=3, strict.vf=TRUE, strong.prefs=NULL,
                                                                 weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                                 which.attributes = c(1,1,1,1), greater.than.one = TRUE, hierarchy.data=hierarchy.data)
  #print(improvement[["nodes11"]]$result)
  #print(improvement[["nodes12"]]$result)
  #print(improvement[["nodes1"]]$result)
  #print(improvement[["nodes2"]]$result)
  #print(improvement[["nodes"]]$result)
  #print("- - - - - - - - - - - - - -")
  
  expect_that(improvement[["nodes"]]$result,is_more_than(7))
  expect_that(improvement[["nodes"]]$result,is_less_than(7.1))
  improvement <- findNecessaryRankRelatedPerformanceModificationHierarchical(perf=performances, a=6, k=1, strict.vf=TRUE, strong.prefs=NULL,
                                                                 weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                                 which.attributes = c(1,1,1,1), greater.than.one = FALSE, hierarchy.data=hierarchy.data)
  #print(improvement[["nodes11"]]$result)
  #print(improvement[["nodes12"]]$result)
  #print(improvement[["nodes1"]]$result)
  #print(improvement[["nodes2"]]$result)
  #print(improvement[["nodes"]]$result)
  #print("- - - - - - - - - - - - - -")
  expect_that(improvement[["nodes"]]$result,is_more_than(1))
  expect_that(improvement[["nodes"]]$result,is_less_than(1.01))
  
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
  improvement <- findNecessaryRankRelatedPerformanceModificationHierarchical(perf=performances, a=6, k=6, strict.vf=TRUE, strong.prefs=NULL,
                                                                 weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                                 which.attributes = c(1,1,1,1), greater.than.one = FALSE, hierarchy.data=hierarchy.data)
  
  #print(improvement[["nodes11"]]$result)
  #print(improvement[["nodes12"]]$result)
  #print(improvement[["nodes1"]]$result)
  #print(improvement[["nodes2"]]$result)
  #print(improvement[["nodes"]]$result)
  #print("- - - - - - - - - - - - - -")
  expect_that(improvement[["nodes"]]$result,is_more_than(0.095))
  expect_that(improvement[["nodes"]]$result,is_less_than(0.105))
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
  improvement <- findNecessaryRankRelatedPerformanceModificationHierarchical(perf=performances, a=6, k=2, strict.vf=TRUE, strong.prefs=NULL,
                                                                 weak.prefs=NULL, indif.prefs = NULL,
                                                                 strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                                 rank.related.requirements = NULL,  nums.of.characteristic.points=NULL, precision=0.005,
                                                                 which.attributes = c(1,1,1,1), greater.than.one = FALSE, hierarchy.data=hierarchy.data)
  
  #print(improvement[["nodes11"]]$result)
  #print(improvement[["nodes12"]]$result)
  #print(improvement[["nodes1"]]$result)
  #print(improvement[["nodes2"]]$result)
  #print(improvement[["nodes"]]$result)
})


