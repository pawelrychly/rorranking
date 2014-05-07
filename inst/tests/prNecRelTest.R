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
  colnames(performances)=c("g1", "g2", "g3", "g4");
  return(performances);
}

test_that("Preference reducts for nec relations no characteristic points", {
  strong.preferences <-  matrix(c(2,3,
                                  4,3,
                                  6,5),
                                ncol=2,
                                byrow=TRUE)

  performances <- getPerformances2();
  results <- PrFindPreferentionalReductsForNecessaryRelations(perf=performances, strong.prefs=strong.preferences, strict.vf=TRUE, nums.of.characteristic.points=c());
  reducts=results$reducts
  expect_that(reducts[["2 >=^N 1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["2 >=^N 3"]][[1]],is_equivalent_to("2 > 3"))
  expect_that(reducts[["2 >=^N 5"]][[1]],is_equivalent_to("2 > 3"))
  expect_that(reducts[["3 >=^N 1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["3 >=^N 5"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["4 >=^N 1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["4 >=^N 3"]][[1]],is_equivalent_to("4 > 3"))
  expect_that(reducts[["4 >=^N 5"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["5 >=^N 1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["6 >=^N 1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["6 >=^N 2"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["6 >=^N 3"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["6 >=^N 4"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["6 >=^N 5"]],is_equivalent_to(" EMPTY SET "))
  #expect_that(ranks[2,2],is_more_than(ranks[3,1]))
  print("Preference reducts for nec relations (no characteristic points) OK")
})

test_that("Preference reducts for nec relations characteristic points: c(2,2,2,2)", {
  strong.preferences <-  matrix(c(2,3,
                                  4,3,
                                  6,5),
                                ncol=2,
                                byrow=TRUE)
  performances <- getPerformances2();
  results <- PrFindPreferentionalReductsForNecessaryRelations(perf=performances, strong.prefs=strong.preferences, strict.vf=TRUE, nums.of.characteristic.points=c(2,2,2,2));
  reducts=results$reducts
  expect_that(reducts[["2 >=^N 1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["2 >=^N 3"]][[1]],is_equivalent_to("2 > 3"))
  expect_that(reducts[["2 >=^N 5"]][[1]],is_equivalent_to("2 > 3"))
  expect_that(reducts[["3 >=^N 1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["3 >=^N 5"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["4 >=^N 1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["4 >=^N 2"]][[1]],is_equivalent_to("2 > 3"))
  expect_that(reducts[["4 >=^N 2"]][[2]],is_equivalent_to("4 > 3"))
  expect_that(reducts[["4 >=^N 3"]][[1]],is_equivalent_to("2 > 3"))
  expect_that(reducts[["4 >=^N 3"]][[2]],is_equivalent_to("4 > 3"))
  expect_that(reducts[["4 >=^N 5"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["5 >=^N 1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["6 >=^N 1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["6 >=^N 2"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["6 >=^N 3"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["6 >=^N 4"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["6 >=^N 5"]],is_equivalent_to(" EMPTY SET "))
  print("Preference reducts for nec relations (characteristic points c(2,2,2,2)) OK")
})


