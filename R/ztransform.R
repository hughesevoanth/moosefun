#' A Function to Z transform a vector of data
#'
#' A Function to Z transform a vector of data
#' @param formula ztransformation formula
#' @param data the vector of data to be transformed
#' @param family the distribution family used in the ztransformation 
#' @keywords correlation analysis among factors
#' @export
#' @examples
#' ztransform()
ztransform = function (formula, data, family = gaussian) 
{
  if (missing(data)) {
    if (is(formula, "formula")) 
      data <- environment(formula)
    else data <- environment()
  }
  else {
    if (is(data, "gwaa.data")) {
      data <- data@phdata
    }
    else if (!is(data, "data.frame")) {
      stop("data argument should be of gwaa.data or data.frame class")
    }
  }
  if (is.character(family)) 
    family <- get(family, mode = "function", envir = parent.frame())
  if (is.function(family)) 
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }
  if (is(try(formula, silent = TRUE), "try-error")) {
    formula <- data[[as(match.call()[["formula"]], "character")]]
  }
  if (is(formula, "formula")) {
    mf <- model.frame(formula, data, na.action = na.pass, 
                      drop.unused.levels = TRUE)
    mids <- complete.cases(mf)
    mf <- mf[mids, ]
    y <- model.response(mf)
    desmat <- model.matrix(formula, mf)
    lmf <- glm.fit(desmat, y, family = family)
    resid <- lmf$resid
  }
  else if (is(formula, "numeric") || is(formula, "integer") || 
           is(formula, "double")) {
    y <- formula
    mids <- (!is.na(y))
    y <- y[mids]
    resid <- y
    if (length(unique(resid)) == 1) 
      stop("trait is monomorphic")
    if (length(unique(resid)) == 2) 
      stop("trait is binary")
  }
  else {
    stop("formula argument must be a formula or one of (numeric, integer, double)")
  }
  y <- (resid - mean(resid))/sd(resid)
  tmeas <- as.logical(mids)
  out <- rep(NA, length(mids))
  out[tmeas] <- y
  out
}

