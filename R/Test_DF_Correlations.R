#' A Function estimate the correlation among all columns in a data frame, categorical and quantitative
#'
#' This function estimates the correlation structure among all columns in a data frame
#' @param df a data frame of data, can contain both numeric (quantitative) and factor (categorical) data
#' @keywords correlation analysis among factors
#' @export
#' @examples
#' Test_DF_Correlations()
Test_DF_Correlations = function(df){
  rhomat = pvalmat = matrix(NA, ncol(df), ncol(df))
  diag(rhomat) = diag(pvalmat) = 1
  for(i in 1:ncol(df)){
    for(j in 1:ncol(df)){
      x = unlist(df[,i]);  y = unlist(df[,j])
      if(class(x) == "factor" & class(y) == "factor" ){
        test = factor_on_factor(x,y)
        rhomat[i,j] = test[1]
        pvalmat[i,j] = test[3]
      } else{
        if(class(x) == "numeric" & class(y) == "numeric"){
          test = numeric_on_numeric(x,y)
          rhomat[i,j] = test[1]
          pvalmat[i,j] = test[2]
        }else{
          if( class(x) == "factor"){
            test = factor_on_numeric(cat_values = x , num_values = y)
            rhomat[i,j] = test[2] ### rho
            pvalmat[i,j] = test[4] ### pval
          } else {
            test = factor_on_numeric(cat_values = y , num_values = x)
            rhomat[i,j] = test[2] ### rho
            pvalmat[i,j] = test[4] ### pval
            }
          }
        }
      }
  }
  out = list(RhoMat = rhomat, PvalMat = pvalmat)
  return(out)
  }

#   Test_DF_Correlations = function(df){
#   rhomat = pvalmat = matrix(NA, ncol(df), ncol(df))
#   diag(rhomat) = diag(pvalmat) = 1
#   for(i in 1:ncol(df)){
#     for(j in 1:ncol(df)){
#       x = unlist(df[,i]);  y = unlist(df[,j])
#       if( class(x) == "factor" & class(y) == "factor" ){
#         test = factor_on_factor(x,y)
#         rhomat[i,j] = test[1]
#         pvalmat[i,j] = test[3]
#       } else{
#         if(class(x) == "numeric" & class(y) == "numeric"){
#           test = numeric_on_numeric(x,y)
#           rhomat[i,j] = test[1]
#           pvalmat[i,j] = test[2]
#         }else{
#           if( class(x) == "factor" & class(y) == "numeric"){
#             test = factor_on_numeric(cat_values = x , num_values = y)
#             rhomat[i,j] = test[1]
#             pvalmat[i,j] = test[2]
#           } else {
#               if(class(x) == "numeric" & class(y) == "factor"){
#                 test = factor_on_numeric(cat_values = y , num_values = x)
#                 rhomat[i,j] = test[1]
#                 pvalmat[i,j] = test[2]
#               } else {
#                 rhomat[i,j] = NA
#                 pvalmat[i,j] = NA
#               }
#             }
#           }
#         }
#       }
#   }
#   out = list(RhoMat = rhomat, PvalMat = pvalmat)
#   return(out)
#   }
