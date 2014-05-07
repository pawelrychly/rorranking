getPerformancesHierarchical <- function() {
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

performances <- getPerformancesHierarchical()
hierarchy.data <- getHierarchyData()
#getCriteriaByNodesMatrix(performances, hierarchy.data[["criteria.by.nodes"]])
#nodes <- getListOfNodesFromHierarchy(hierarchy.data[["hierarchy.tree"]])

#if 5 > 2 then 3 > 2

#strong.preference = matrix(c(5,2), ncol=2,byrow=TRUE)  
#ranks <- ExtremeRankingAnalysisHierarchical(perf = performances, 
#                                strict.vf = TRUE, 
#                                strong.prefs = strong.preference, 
#                                strong.intensities.of.prefs = NULL, 
#                                rank.related.requirements = NULL,nums.of.characteristic.points=c(), hierarchy.data=hierarchy.data) 

#print(ranks)
#preference.intensities =  matrix(c("a1","a3", "a4","a2", "nodes1"),ncol=5, byrow=TRUE)
#preference.intensities = matrix(c(4,3, 4,2,0),ncol=5, byrow=TRUE)
#print(preference.intensities)
#print(performances)
#performances <- getPerformancesHierarchical()
#hierarchy.data <- getHierarchyData()


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

indif.preference = matrix(c("a5","a3"), ncol=2,byrow=TRUE)  
strong.preference =  matrix(c("a4","a3", "nodes1"), ncol=3,byrow=TRUE)  

relations <- findNecessaryAndPossiblePreferenceRelationsHierarchical(perf = performances, 
                                                       strong.prefs = strong.preference, weak.prefs = NULL, indif.prefs = indif.preference, 
                                                       strict.vf=FALSE,
                                                       nums.of.characteristic.points = NULL, hierarchy.data = hierarchy.data)

nec.relations <- relations$nec.relations
results <- findPreferentionalReductsForNecessaryRelationsHierarchical(perf = performances, 
                                                            strong.prefs = strong.preference, weak.prefs = NULL, indif.prefs =indif.preference, 
                                                            strict.vf=FALSE,
                                                            nums.of.characteristic.points = NULL, nec.relations=nec.relations, hierarchy.data=hierarchy.data)
#print(results["nodes11"])
#strong.preference =  matrix(c("a4","a3", "nodes1"), ncol=3,byrow=TRUE)  
#indif.preference = matrix(c("a5","a3", "nodes2"), ncol=3,byrow=TRUE)
#hierarchy.data <- getHierarchyData()
#ranks <- extremeRankingAnalysisHierarchical(perf = performances, 
#                                strict.vf = FALSE, 
#                                strong.prefs = NULL, weak.prefs = NULL, indif.prefs = NULL,
#                                nums.of.characteristic.points=NULL, hierarchy.data=hierarchy.data)
#print(ranks$nodes2)
#ranks <- extremeRankingAnalysisHierarchical(perf = performances, 
#                                strict.vf = FALSE, 
#                                strong.prefs = strong.preference, weak.prefs = NULL, indif.prefs = indif.preference,
#                                nums.of.characteristic.points=NULL, hierarchy.data=hierarchy.data)
#

#print(ranks$nodes2)

#reducts <- findAllRankRelatedPreferentionalReductsHierarchical(perf=performances, ranks=ranks, strict.vf=FALSE, strong.prefs = strong.preference, weak.prefs = NULL,
#                                                   indif.prefs =  indif.preference,  nums.of.characteristic.points = NULL, hierarchy.data =hierarchy.data)
#print(reducts$nodes2)