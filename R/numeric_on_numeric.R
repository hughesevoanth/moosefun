#' A Function to describe each feature in a data frame of metabolite data. 
#'
#' This function estimates the correlation structure among two factors, using a chi-square test and the Crammer's V statsitic
#' @param x a vector of class numeric
#' @param c a vector of class numeric
#' @keywords correlation analysis among factors
#' @export
#' @examples
#' numeric_on_numeric()
numeric_on_numeric = function( x , y ){
  sptest = cor.test( unlist(x) , unlist(y), method = "sp")
  ##
  out = c( sptest$estimate , sptest$p.value)
  names(out) = c("rho",  "Sp_rho_pval")
  return(out)
}
