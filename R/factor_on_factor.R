#' A Function to describe each feature in a data frame of metabolite data. 
#'
#' This function estimates the correlation structure among two factors, using a chi-square test and the Crammer's V statsitic
#' @param x a vector of class factor
#' @param y a second vector of class factor
#' @keywords correlation analysis among factors
#' @export
#' @examples
#' factor_on_factor()
factor_on_factor = function(x,y){
  mytable = table(x,y)
  chi2test = chisq.test(mytable, correct=F)
  N = sum(chi2test$observed)
  chi2 = chi2test$statistic
  pval = chi2test$p.value
  ## summary stats
  k <- min(dim(chi2test$observed))
  V <- sqrt(chi2/(N * (k - 1)))
        
  out = c(V, chi2, pval)
  names(out) = c("Crammers_V", "Chi2_stat", "Chi2_pval")
  return(out)
}
