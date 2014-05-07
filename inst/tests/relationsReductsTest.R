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


test_that("Finding preferentional reducts work properly. ", {
 
  performances <- matrix(c(1,1,1,1,
                           9,2,3,1,
                           5,5,5,1,
                           9,1,5,1,
                           5,5,3,1,
                           10,10,10,2), 
                         ncol=4, 
                         byrow=TRUE);
  rownames(performances)=c("a1", "a2", "a3", "a4", "a5", "a6");
  colnames(performances)=c("c01", "c02", "c03", "c04");
  
  strong.preference = matrix(c("a4","a3"), ncol=2,byrow=TRUE)  
  indif.preference = matrix(c("a5","a3"), ncol=2,byrow=TRUE)  
  
  
  relations <- findNecessaryAndPossiblePreferenceRelations(perf = performances, 
                                                           strong.prefs = strong.preference, weak.prefs = NULL, indif.prefs = indif.preference, 
                                                           strict.vf=FALSE,
                                                           nums.of.characteristic.points = NULL)
  
  #print(relations)
  nec.relations <- relations$nec.relations
  reducts <- findPreferentionalReductsForNecessaryRelations(perf = performances, 
                                                            strong.prefs = strong.preference, weak.prefs = NULL, indif.prefs =indif.preference, 
                                                            strict.vf=FALSE,
                                                            nums.of.characteristic.points = NULL, nec.relations=nec.relations)

  
  expect_that(reducts[["a2 >=^N a1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["a2 >=^N a3"]][[1]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts[["a2 >=^N a3"]][[1]][[2]],is_equivalent_to("a5 == a3"))
  expect_that(reducts[["a4 >=^N a3"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts[["a5 >=^N a3"]][[1]], is_equivalent_to("a5 == a3"))
  
  
  #expect_that(ranks[2,2],is_more_than(ranks[3,1]))
})

test_that("Finding preferentional reducts in hierarchical version work properly.", {
  hierarchy.data <- getHierarchyData()
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
  indif.preference = matrix(c("a5","a3"), ncol=2,byrow=TRUE)  
  
  relations <- findNecessaryAndPossiblePreferenceRelationsHierarchical(perf = performances, 
                                                                       strong.prefs = strong.preference, weak.prefs = NULL, indif.prefs = indif.preference, 
                                                                       strict.vf=FALSE,
                                                                       nums.of.characteristic.points = NULL, hierarchy.data = hierarchy.data)
  
  nec.relations <- relations$nec.relations
  #print(nec.relations[["nodes11"]])
  reducts <- findPreferentionalReductsForNecessaryRelationsHierarchical(perf = performances, 
                                                                        strong.prefs = strong.preference, weak.prefs = NULL, indif.prefs =indif.preference, 
                                                                        strict.vf=FALSE,
                                                                        nums.of.characteristic.points = NULL, nec.relations=nec.relations, hierarchy.data=hierarchy.data)
  
  expect_that(reducts[["nodes11"]][["a2 >=^N a1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["nodes11"]][["a2 >=^N a3"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["nodes11"]][["a2 >=^N a4"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["nodes11"]][["a2 >=^N a5"]],is_equivalent_to(" EMPTY SET "))
  
  expect_that(reducts[["nodes11"]][["a3 >=^N a1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["nodes11"]][["a3 >=^N a5"]],is_equivalent_to(" EMPTY SET "))
  
  expect_that(reducts[["nodes11"]][["a4 >=^N a1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["nodes11"]][["a4 >=^N a2"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["nodes11"]][["a4 >=^N a3"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["nodes11"]][["a4 >=^N a5"]],is_equivalent_to(" EMPTY SET "))
  
  expect_that(reducts[["nodes11"]][["a5 >=^N a1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["nodes11"]][["a5 >=^N a3"]],is_equivalent_to(" EMPTY SET "))
  
  expect_that(reducts[["nodes11"]][["a6 >=^N a1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["nodes11"]][["a6 >=^N a2"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["nodes11"]][["a6 >=^N a3"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["nodes11"]][["a6 >=^N a4"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["nodes11"]][["a6 >=^N a5"]],is_equivalent_to(" EMPTY SET "))
                          
  expect_that(reducts[["nodes"]][["a2 >=^N a1"]],is_equivalent_to(" EMPTY SET "))
  expect_that(reducts[["nodes"]][["a2 >=^N a3"]][[1]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts[["nodes"]][["a2 >=^N a3"]][[1]][[2]],is_equivalent_to("a5 == a3"))
  expect_that(reducts[["nodes"]][["a4 >=^N a3"]][[1]],is_equivalent_to("a4 > a3"))
  expect_that(reducts[["nodes"]][["a5 >=^N a3"]][[1]],is_equivalent_to("a5 == a3"))  
  
  
  weak.preference = matrix(c("a4","a3", "nodes1"), ncol=3,byrow=TRUE)  
  
  relations <- findNecessaryAndPossiblePreferenceRelationsHierarchical(perf = performances, 
                                                                       strong.prefs = NULL, weak.prefs = weak.preference, indif.prefs = NULL, 
                                                                       strict.vf=FALSE,
                                                                       nums.of.characteristic.points = NULL, hierarchy.data = hierarchy.data)
  
  nec.relations <- relations$nec.relations
  #print(nec.relations[["nodes1"]])
  reducts <- findPreferentionalReductsForNecessaryRelationsHierarchical(perf = performances, 
                                                                        strong.prefs = NULL, weak.prefs = weak.preference, indif.prefs = NULL, 
                                                                        strict.vf=FALSE,
                                                                        nums.of.characteristic.points = NULL, nec.relations=nec.relations, hierarchy.data=hierarchy.data)
  expect_that(reducts[["nodes1"]][["a4 >=^N a5"]][[1]],is_equivalent_to("a4 >= a3[nodes1]"))
  expect_that(reducts[["nodes1"]][["a4 >=^N a3"]][[1]],is_equivalent_to("a4 >= a3[nodes1]"))  
})


