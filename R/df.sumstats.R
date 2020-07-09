#' A Function to describe each feature in a data frame of metabolite data. 
#'
#' This function allows you to generate summary statistics for metabolite features using the describe() function from the psych package, as well as estimate variance, a dispersion index, the coeficent of variation, proportion of zeros in data, and shapiro's W-statistic.
#' @param wdata the metabolite data drame samples in row, metabolites in columns
#' @keywords metabolome, microbiome
#' @export
#' @examples
#' df.sumstats()
df.sumstats = function(wdata){
  out = apply(wdata, 2, function(x){
  ## proportion NA
  propNA = sum(is.na(x))/length(x)

  ## insure data is numeric
  x = as.numeric( na.omit(x) )
  if(length(x) < 25 ){
    o = rep(0, 18)
    names(o) = c("vars","n","mean","sd", "median","trimmed","mad",
      "min","max","range","skew","kurtosis","se","dispersionindex",
      "coefvar","propzero","propNA", "Wstat" )
    } else {

  ## Variance
  v = var(x, na.rm = TRUE); if( is.na(v) ){v = 0}

  ## dispersion index
  dispersionindex = v / mean(x, na.rm = T)
  if(v == 0){dispersionindex = NA}

  ## coef of var
  coefvar = sd(x, na.rm = T) / mean(x, na.rm = T)

  ## proportion zero
  propzero = sum(x == 0)/length(x)
  
  ## describe data using psych package function
  d = psych::describe(x)
  
  ## W-statistic
    ## shorten if too long
    if(length(x) > 5000 ){
      x = sample(x, 5000, replace = FALSE)
    }
    ## Do not run for those with zero variance
    if(d$sd == 0){ 
      Wstat = NA 
      } else {
        Wstat = shapiro.test(x)$stat
      }
  ## out data
  o = unlist( cbind(d, dispersionindex, coefvar, propzero, propNA, Wstat) )
}
return(o)
})
  ## turn data matrix into a tibble
  out = as.data.frame( t(out) )
  out =  cbind(rownames(out), out)
  colnames(out)[1] = "ID"
  return( tibble::as_tibble( out ) )
}


