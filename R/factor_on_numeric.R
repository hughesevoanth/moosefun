#' A Function to describe each feature in a data frame of metabolite data. 
#'
#' This function estimates the correlation structure among a factor and a numeric|quantitative trait
#' @param cat_values a vector of class factor, categorical values
#' @param num_values a second vector of class numeric
#' @keywords correlation analysis among factors
#' @export
#' @examples
#' factor_on_numeric()
factor_on_numeric = function( cat_values , num_values ){
  ## if values are integers, do not rntransform
  if(class(num_values) == "numeric"){
    num_values = rntransform( unlist(num_values) ) 
  }
  ###
  wdata = data.frame( cat_values = unlist(cat_values) , num_values = unlist(num_values) )
  fit = try( lm(num_values ~ cat_values , data = wdata) )
  if(length(fit)>1){
    adjrsq = summary(fit)$adj.r.squared
    ####
    a = anova(fit)
    etasq = a[1,2]/sum(a[,2])
    rho = sqrt(etasq)
    fstat = a[1,4]
    pval = a[1,5]
    out = c( etasq , rho,  fstat, pval)
  } else {
    out = c(0,0,0,1)
  }
  names(out) = c("Etasq", "rho", "Fstat", "Anova_pval")
  return(out)
}
