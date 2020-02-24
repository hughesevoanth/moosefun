#' A function to identify and extract the principal variables in your dataset. 
#'
#' A function to identify and extract the principal variables in your dataset. The function will iterate over each newly generated list of PVs to insure that they do meet your tree cutheight standards.
#' @param variabledata a data.frame of the variable data used to build your cormat and hclust tree
#' @param tree a hclust object of you variables, built with the cormat and the hclust method "complete"
#' @param cormat your data.frame of variables used to generate your tree
#' @param cutheight the height at which you wish to cut your tree 
#' @keywords principal variables, tree cut
#' @export
#' @examples
#' ind.pvs()
ind.pvs = function( variabledata, tree, cormat, cutheight){
  nomoreclusters = 0
  ### while loop
  ### while there are still clusters, at height h, in each newly generated tree using the PVs
  ### thencut tree and make new PVs again. 
  i = 1
  while(nomoreclusters == 0){
    i = i+1
    cat("Tree cut and PV identifier iteration ", i, "\n")
    ## identify the Principal Variables (PVs)
    PVs = treecut.pvs( tree = tree, variabledata = variabledata, cutheight = cutheight)
    n = as.character(PVs[, 3])
    newdistmat = as.dist(as.matrix(tdat$distmat)[n, n])
    tree = hclust(newdistmat,  method = "complete")
    k = table( cutree(tree, h = cutheight) )
    ###############
    if( length(n) == length( k ) ){
      nomoreclusters = 1
    }
    ###############
  }
  #### estimate the maximum correlation that remains in the data set
  n = as.character(PVs[, 3])
  MaxCor_remaining = max( as.dist( abs( cormat[n,n] ) ) , na.rm = TRUE)
  ###
  out = list(PVs = PVs, tree = tree, cutheight = cutheight, MaxCor_remaining = MaxCor_remaining)
  return(out)
}