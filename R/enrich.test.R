#' A simple hypergeomeric enrichment test
#'
#' This function performs a hypergeometric test on each unique class of variable in a list. 
#' @param sig a vector of variables for all "significant" observations
#' @param background a vector of variables for all tested observations
#' @keywords fisher exact test, hyper geometric test
#' @export
#' @examples
#' enrich.test()
enrich.test <- function(background, sig) {
  ## a vector of all unique variables to
  ## be tested for enrichment
  unique_sig_vars <- unique(na.omit(sig))
  ## iterate over this vector
  etest <- sapply(unique_sig_vars, function(x) {
    a <- sum(sig == x, na.rm = TRUE)
    b <- length(sig) - a
    ##
    d <- sum(background == x, na.rm = TRUE)
    e <- length(background) - d
    ##
    testmat <- matrix(c(a, b, d, e), 2, 2, byrow = FALSE)
    ##
    colnames(testmat) <- c("sig", "not_sig")
    rownames(testmat) <- c("in_cat", "out_cat")
    ##
    f <- fisher.test(testmat)
    out <- c(f$estimate, f$p.value)
    names(out) <- c("OR", "pval")
    return(out)
  })
  return(t(etest))
}

