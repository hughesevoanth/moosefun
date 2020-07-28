#' A kmeans and Silhouette Analysis to determine the best K for your data
#'
#' This function estimates the best K (number of clusters) for your data set using Kmeans and a Silhouette Analysis
#' @param mydata a data frame or matrix of data
#' @param kmax the maximum number of k groups to estimate
#' @param desiredK the the number of k clusters you want to have
#' @param num_iter the number of iterations used in the kmeans analysis
#' @param plotrepress default is to plot the data (FALSE), set to TRUE to repress the plot
#' @keywords kmeans, silhouette
#' @export
#' @examples
#' DetermineK()
DetermineK = function(mydata, kmax = 20, desiredK = 4, num_iter = 100, plotrepress=FALSE){
  ## Within Group SumOfSquares
  WSS <- sapply(1:kmax,function(k){kmeans(mydata, centers = k, iter.max = num_iter, nstart = 10 )$tot.withinss})
  ## Silhouette analysis
  sil = sapply(2:kmax, function(k){
    cluster::pam(mydata, k) $ silinfo $ avg.width
  })
  k.sil = which.max(sil) + 1
  ## Silhouette analysis 2
  sil.est = fpc::pamk(mydata, krange=1:kmax)$nc
  ## Plot
  if(plotrepress == 0){
    mat = matrix(c(1,2,3,3), ncol = 2, byrow = FALSE)
    layout(mat)
    plot(1:kmax, WSS, type="b", pch = 19, frame = FALSE,xlab="Number of clusters K", ylab="Tot. Within Clusters SS", main = "Cluster Within Group\nSum of Squares"); abline(v = sil.est , lty =2)
    plot(c(0,sil), type = "b", pch = 19, frame = FALSE, main = "Silhouette Analysis", ylab  = "Avg Silhouette Width"); abline(v = sil.est , lty =2)
    ###
    d <- dist(mydata, method = "euclidean")
    plot(cluster::pam(d, sil.est))
    ##  estiamte the clusters
    K = kmeans(mydata, centers = sil.est, iter.max = num_iter, nstart = 10 )$cluster
    ##
    Krequested = kmeans(mydata, centers = desiredK, iter.max = num_iter, nstart = 10 )$cluster
    
    ## Output
    return(list( BestK = sil.est, clusters = K, clustersRequested = Krequested ))
  }else{
    
    ##  estiamte the clusters
    K = kmeans(mydata, centers = sil.est, iter.max = num_iter, nstart = 10 )$cluster
    ##
    Krequested = kmeans(mydata, centers = desiredK, iter.max = num_iter, nstart = 10 )$cluster
    
    ## Output
    return(list( BestK = sil.est, clusters = K, clustersRequested = Krequested ))
  }
}
