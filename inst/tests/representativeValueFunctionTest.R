library(testthat)


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


test_that("Finding representative value function work properly.", {
  performances <- getPerformances2()
  rank.related <- matrix(c("a3",2,2),ncol=3, byrow=TRUE)
  results <- findNecessaryAndPossiblePreferenceRelations(perf =performances, 
                                                         strict.vf = TRUE,
                                                         strong.prefs = NULL, weak.prefs = NULL,indif.prefs = NULL,
                                                         strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                         rank.related.requirements = rank.related,
                                                         nums.of.characteristic.points=NULL)

  nec.relations = results$nec.relations
  pos.relations = results$pos.relations  
  func <-findRepresentativeValueFunction(perf =performances, 
                                          strict.vf = TRUE,
                                          strong.prefs = NULL, weak.prefs = NULL,indif.prefs = NULL,
                                          strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                          rank.related.requirements = rank.related,
                                          nums.of.characteristic.points=NULL,
                                          nec.relations.matrix=nec.relations,
                                          is.compromise=TRUE);
  
  #DIAGONAL
  expect_that(func[[1]][1,2],is_less_than(func[[1]][2,2]))
  expect_that(func[[1]][2,2],is_less_than(func[[1]][3,2]))
  expect_that(func[[1]][3,2],is_less_than(func[[1]][4,2]))
  
  expect_that(func[[2]][1,2],is_less_than(func[[2]][2,2]))
  expect_that(func[[2]][2,2],is_less_than(func[[2]][3,2]))
  expect_that(func[[2]][3,2],is_less_than(func[[2]][4,2]))
  
  expect_that(func[[3]][1,2],is_less_than(func[[3]][2,2]))
  expect_that(func[[3]][2,2],is_less_than(func[[3]][3,2]))
  expect_that(func[[3]][3,2],is_less_than(func[[3]][4,2]))
  
  expect_that(func[[4]][1,2],is_less_than(func[[4]][2,2]))
  expect_that(func[[4]][2,2],is_less_than(func[[4]][3,2]))
  expect_that(func[[4]][3,2],is_less_than(func[[4]][4,2]))
  
  
  #print("Finding neccessary and possible relations OK")
})

test_that("Finding representative value function work properly - with costs.", {
  performances <- getPerformances2()
  rank.related <- matrix(c("a3",5,5),ncol=3, byrow=TRUE)
  criteria <- c("c", "c", "c", "c")
  results <- findNecessaryAndPossiblePreferenceRelations(perf =performances, 
                                                         strict.vf = TRUE,
                                                         strong.prefs = NULL, weak.prefs = NULL,indif.prefs = NULL,
                                                         strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                         rank.related.requirements = rank.related,
                                                         nums.of.characteristic.points=NULL, criteria=criteria)

  nec.relations = results$nec.relations
  pos.relations = results$pos.relations  
  func <-findRepresentativeValueFunction(perf =performances, 
                                          strict.vf = TRUE,
                                          strong.prefs = NULL, weak.prefs = NULL,indif.prefs = NULL,
                                          strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                          rank.related.requirements = rank.related,
                                          nums.of.characteristic.points=NULL,
                                          nec.relations.matrix=nec.relations,
                                          is.compromise=TRUE, criteria=criteria);
  
  #DIAGONAL
  expect_that(func[[1]][1,2],is_more_than(func[[1]][2,2]))
  expect_that(func[[1]][2,2],is_more_than(func[[1]][3,2]))
  expect_that(func[[1]][3,2],is_more_than(func[[1]][4,2]))
  
  expect_that(func[[2]][1,2],is_more_than(func[[2]][2,2]))
  expect_that(func[[2]][2,2],is_more_than(func[[2]][3,2]))
  expect_that(func[[2]][3,2],is_more_than(func[[2]][4,2]))
  
  expect_that(func[[3]][1,2],is_more_than(func[[3]][2,2]))
  expect_that(func[[3]][2,2],is_more_than(func[[3]][3,2]))
  expect_that(func[[3]][3,2],is_more_than(func[[3]][4,2]))
  
  expect_that(func[[4]][1,2],is_more_than(func[[4]][2,2]))
  expect_that(func[[4]][2,2],is_more_than(func[[4]][3,2]))
  expect_that(func[[4]][3,2],is_more_than(func[[4]][4,2]))
})


test_that("Finding representative value function work properly - with costs and characteristic points.", {
  performances <- getPerformances2()
  rank.related <- NULL #matrix(c("a3",5,3),ncol=3, byrow=TRUE)
  criteria <- c("c", "c", "c", "c")
  characteristic.points <- c(3,3,3,3)
  results <- findNecessaryAndPossiblePreferenceRelations(perf =performances, 
                                                         strict.vf = TRUE,
                                                         strong.prefs = NULL, weak.prefs = NULL,indif.prefs = NULL,
                                                         strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                                         rank.related.requirements = rank.related,
                                                         nums.of.characteristic.points=characteristic.points, criteria=criteria)
  
  nec.relations = results$nec.relations
  pos.relations = results$pos.relations  
  func <-findRepresentativeValueFunction(perf =performances, 
                                          strict.vf = TRUE,
                                          strong.prefs = NULL, weak.prefs = NULL,indif.prefs = NULL,
                                          strong.intensities.of.prefs = NULL, weak.intensities.of.prefs = NULL, indif.intensities.of.prefs = NULL, 
                                          rank.related.requirements = rank.related,
                                          nums.of.characteristic.points=characteristic.points,
                                          nec.relations.matrix=nec.relations,
                                          is.compromise=TRUE, criteria=criteria);
  
  #DIAGONAL
  expect_that(func[[1]][1,2],is_more_than(func[[1]][2,2]))
  expect_that(func[[1]][2,2],is_more_than(func[[1]][3,2]))
  
  expect_that(func[[2]][1,2],is_more_than(func[[2]][2,2]))
  expect_that(func[[2]][2,2],is_more_than(func[[2]][3,2]))
  
  expect_that(func[[3]][1,2],is_more_than(func[[3]][2,2]))
  expect_that(func[[3]][2,2],is_more_than(func[[3]][3,2]))
  
  expect_that(func[[4]][1,2],is_more_than(func[[4]][2,2]))
  expect_that(func[[4]][2,2],is_more_than(func[[4]][3,2]))
})





