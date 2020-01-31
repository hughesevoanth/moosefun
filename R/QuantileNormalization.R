#' A function to perform a quantile normalisation across a data.frame
#'
#' This function performs a standard quantile normalization often seen in things like gene expression data.
#' @param df a data frame of integer or numerical values
#' @keywords qqnorm, quantile normalization
#' @export
#' @examples
#' quantile_normalisation()
quantile_normalisation <- function(df){
  #df_rank <- apply(df,2,rank,ties.method="min")
  df_rank <- apply(df,2,rank,ties.method="random")
  df_sorted <- data.frame(apply(df, 2, sort))
  df_mean <- apply(df_sorted, 1, mean)
   
  index_to_mean <- function(my_index, my_mean){
    return(my_mean[my_index])
  }
   
  df_final <- apply(df_rank, 2, index_to_mean, my_mean=df_mean)
  rownames(df_final) <- rownames(df)
  return(df_final)
}




