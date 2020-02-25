#' A super function that runs through all of the necessary staeps for you to identify Independent Principal Variables
#'
#' A super function that runs through all of the necessary staeps for you to identify Independent Principal Variables
#' @param variabledata a data.frame of the variable data used to build your cormat and hclust tree
#' @param cutheight tree cut height. Value is equal to a dissimilarity of 1 - Spearman's rho.
#' @keywords data reduction, independent variables, principal variables, tree cut
#' @export
#' @examples
#' iPVs()
iPVs = function( variabledata, cutheight = 0.5 ){
  ## estiamte correlation matrix, build tree, generate PCA from correlation matrix
  wdata = tree.builder(variabledata)
  
  ## identify the PVs (independent principal variables)
  StudyPVs = ind.pvs( variabledata = wdata$variabledata,
  tree = wdata$tree,
  cormat = wdata$cormat,
  distmat = wdata$distmat,
  cutheight = cutheight )
  
  initial_ind_PVs = as.character( StudyPVs$PVs$pvs[,"PV"] )

  ## identify all variables that belong to a group. i.e. each PV is tagging which other variables? 
  PV_cluster_members = Kcluster.groups( ind_pv_iterations = StudyPVs$treecut_iterations )
  
  ## re-estiamte PV and the VarExp by that top PV for the total variation of group members.
  NewPV = Kcluster_PVs(variabledata = wdata$variabledata, Kmembers = PV_k_members )
  final_ind_PVs = as.character(NewPV$PVtable[,1])
  vexp = NewPV$PVtable[,2]
  
  ## place all of the useful data into a table
  clustersize = unlist( lapply(PV_cluster_members, length) )
  groupmembers = unlist( lapply(PV_cluster_members, function(x){ paste(x, collapse = ":") } ) )
  
  iPV_table = data.frame(PVs = final_ind_PVs , 
    clustersize = clustersize,
    VarExp_by_PV = vexp,
    groupmembers = groupmembers )

  ### data out
  return(iPV_table)

}