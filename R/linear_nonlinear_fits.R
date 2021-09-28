#' A function to fit a linear and non-linear GAM model to one's data and return some useful summary statistics
#'
#' This function estimates the best K (number of clusters) for your data set using Kmeans and a Silhouette Analysis
#' @param wdata a data frame of data with appropriate column names
#' @param dependent a string that matches a column name in wdata that you would like to define as the dependent or response variable in your analysis
#' @param independent a string that matches a column name in wdata that you would like to define as the independent or primary explanatory variable of interest in your analysis
#' @param covariables a string or character vector that matches column names in wdata that you would like to define as additional covariate in your model. Set as NA, if you have no covariates and thus would like to run a univariate analysis. 
#' @param rnt_dependent TRUE or FALSE, would you like to rank normal transform your dependent variable prior to fitting the data? Default value is TRUE. Uses the rntransform() function in this package to rank normal transform. 
#' @param bam if you would like to run the GAM with bam(), for very large data sets, then set bam = TRUE. Default is bam = FALSE and gam() is used.
#' @param nthread_count number of compute threads to use in your bam(). 
#' @keywords GAM, non-linear model, linear model, mgcv
#' @export
#' @examples
#' linear_nonlinear_fits()
linear_nonlinear_fits = function(wdata, dependent, independent, covariables = NA, rnt_dependent = TRUE, bam = FALSE, nthread_count = 1){
	##############################
	### 1. Define Model Data Frame
	##############################
  	if(is.na(covariables[1])){
    	model_variables = c( dependent, independent )
  	} else {
    	model_variables = c( dependent, covariables, independent)  
  	}

  	mod_data = wdata[, c(model_variables)]

  	##############################
	  ### 2. Rank Transform dependent
	  ###    if desired
	  ##############################
  	if(rnt_dependent == TRUE){
  		mod_data[, dependent] = rntransform(mod_data[, dependent])
  	}

  	##############################
	  ### 3. Define Linear Model formula
	  ##############################
  	if( is.na( covariables[1] ) ){
      	form = formula(paste0(dependent ," ~ ", independent ))  
    	} else {
      	form = formula(paste0(dependent ," ~ ", paste0(covariables, collapse = " + ") ," + ", independent ))  
    	}

  	##############################
	  ### 4. Perform Linear Model 
	  ##############################
  	if(bam == TRUE){
  	  lm_mod = mgcv::bam(form, data = mod_data, method = "REML", nthreads = nthread_count)  
  	} else {
  	  lm_mod = mgcv::gam(form, data = mod_data, method = "REML")  
  	}
  	# glm_mod = glm(form, data = mod_data, family = "gaussian")
  	
  	#################
  	## 4a. summary stats
  	#################
  	s = summary(lm_mod)
  	## sample size
  	n = s$n; names(n) = "n_lm"
  	## Adjusted R-squared, to compare between models with a different number of predictors
  	rsq = s$r.sq; names(rsq) = "rsq_adj_lm"
  	## Deviance explained
  	dexp = s$dev.expl; names(dexp) = "dev_exp_lm"
  	## Dependent Variable effect estimates
  	beta = s$p.coeff[independent]; names(beta) = "beta_lm"
  	se = s$se[independent]; names(se) = "se_lm"
  	tval = s$p.t[independent]; names(tval) = "tval_lm"
  	pval = s$p.pv[independent]; names(pval) = "P_lm"
  	loglik_lm = logLik.gam(lm_mod); names(loglik_lm) = "loglik_lm"
  	aic_lm = AIC(lm_mod); names(aic_lm) = "aic_lm"
    lm_results = c(n, loglik_lm, aic_lm, rsq, dexp, beta, se, tval, pval)
    
  	##############################
  	### 5. Define GAM Model formula
  	##############################
  	if( is.na( covariables[1] ) ){
     	 form = formula(paste0(dependent ," ~ s(", independent, ")" ))  
    	} else {
      	form = formula(paste0(dependent ," ~ ", paste0(covariables, collapse = " + ") ," +  s(", independent, ")" ))  
    	}

  	##############################
	  ### 6. Perform GAM (non-linear) Model 
	  ##############################
    if(bam == TRUE){
      gam_mod = mgcv::bam( form, data = mod_data,  method = "REML", nthreads = nthread_count)  
    } else {
      gam_mod = mgcv::gam( form, data = mod_data,  method = "REML")  
    }
  	# gam.check(gam_mod)

  	#################
  	## 6a. summary stats
  	#################
  	s = summary(gam_mod)
  	## sample size
  	n_gam = s$n; names(n_gam) = "n_gam"
  	## smooth data; removing the reference degrees of freedom
  	smooth_data = s$s.table; names(smooth_data) = c("edf_gam", "Ref_df_gam", "F_gam", "P_gam")
  	## R-squared
  	rsq_gam = s$r.sq; names(rsq_gam) = "rsq_adj_gam"
  	## Deviance Explained
  	dexp_gam = s$dev.expl; names(dexp_gam) = "dev_exp_gam"
  	## GAM coefficients for the independent
  	# coef = gam_mod$coefficients
  	# w = grep( independent, names(coef))
  	# coef = coef[w]
  	# coef = round(coef, d = 4)
  	# coef = paste(coef, collapse = "|"); names(coef) = "coefs_gam"
    
  	## REML LogLiklihood and AIC
  	reml_gam = logLik.gam(gam_mod)[1]; names(reml_gam) = "reml_gam"
  	loglik_gam = logLik.gam(gam_mod); names(loglik_gam) = "loglik_gam"
  	aic_gam = AIC(gam_mod); names(aic_gam) = "aic_gam"
  	
  	## GAM Results out
  	gam_results = c(n_gam, loglik_gam, aic_gam, rsq_gam, dexp_gam, smooth_data )

  	####################################
  	## 7. ANOVA test of lm and GAM
  	####################################
  	a = anova(lm_mod, gam_mod, test = "F")
  	Ftest = c(a$`Resid. Dev`, a$Df[2], a$Deviance[2], a$F[2], a$`Pr(>F)`[2] ); names(Ftest) = c("resid_dev_lm_Ftest","resid_dev_gam_Ftest","df_Ftest","deviance_Ftest", "F_Ftest", "P_Ftest")

  	####################################
  	## 8. Return estimates
  	####################################
  	# out = c( n, rsq, dexp, dep_est, n_gam, rsq_gam, dev_exp, smooth_data, coef, Ftest_P )
  	out = c( lm_results, gam_results, Ftest )
  	
  	return(out)
}
