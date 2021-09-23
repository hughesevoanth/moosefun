#' A function to fit a linear and non-linear GAM model to one's data and return some useful summary statistics
#'
#' This function estimates the best K (number of clusters) for your data set using Kmeans and a Silhouette Analysis
#' @param wdata a data frame of data with appropriate column names
#' @param dependent a string that matches a column name in wdata that you would like to define as the dependent or response variable in your analysis
#' @param independent a string that matches a column name in wdata that you would like to define as the independent or primary explanatory variable of interest in your analysis
#' @param covariables a string or character vector that matches column names in wdata that you would like to define as additional covariate in your model. Set as NA, if you have no covariates and thus would like to run a univariate analysis. 
#' @param rnt_dependent TRUE or FALSE, would you like to rank normal transform your dependent variable prior to fitting the data? Default value is TRUE. Uses the rntransform() function in this package to rank normal transform. 
#' @keywords GAM, non-linear model, linear model, mgcv
#' @export
#' @examples
#' linear_nonlinear_fits()
linear_nonlinear_fits = function(wdata, dependent, independent, covariables = NA, rnt_dependent = TRUE){
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
  	lm_mod = lm(form, data = mod_data)
  	glm_mod = glm(form, data = mod_data, family = "gaussian")
  	
  	#################
  	## 4a. summary stats
  	#################
  	s = summary(lm_mod)
  	## sample size
  	n = length( s$residuals ); names(n) = "n_lm"
  	## R-squared
  	rsq = s$r.squared; names(rsq) = "rsq_lm"
  	## Adjusted R-squared, to compare between models with a different number of predictors
	rsq_adj = s$adj.r.squared; names(rsq_adj) = "rsq_adj_lm"
	## Dependent Variable effect estimates
	dep_est = s$coef[independent, ]; names(dep_est) = c("beta_lm","se_lm","tval_lm","P_lm")

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
  	gam_mod = gam( form, data = mod_data,  method = "REML")  
  	# gam.check(gam_mod)

  	#################
  	## 6a. summary stats
  	#################
  	s = summary(gam_mod)
  	## sample size
  	n_gam = s$n; names(n_gam) = "n_gam"
  	## smooth data; removing the reference degrees of freedom
  	smooth_data = s$s.table[-2]; names(smooth_data) = c("edf_gam", "F_gam", "P_gam")
  	## R-squared
  	rsq_gam = s$r.sq; names(rsq_gam) = "rsq_gam"
  	## Deviance Explained
  	dev_exp = s$dev.expl; names(dev_exp) = "dev_exp_gam"
  	## GAM coefficients
  	coef = gam_mod$coefficients
  	w = grep( independent, names(coef))
  	coef = coef[w]
  	coef = round(coef, d = 4)
  	coef = paste(coef, collapse = "|"); names(coef) = "coefs_gam"
  

  	####################################
  	## 7. ANOVA test of lm and GAM
  	####################################
  	a = anova(lm_mod, gam_mod, test = "F")
  	Ftest_P = c(a$RSS, a$Df[2], a$`Sum of Sq`[2], a$F[2], a$`Pr(>F)`[2]); names(Ftest_P) = c("lm_RSS","gam_RSS","Ftest_df","Ftest_RSS_delta", "Ftest_F", "Ftest_P")

  	####################################
  	## 8. Return estimates
  	####################################
  	out = c( n, rsq, rsq_adj, dep_est, n_gam, rsq_gam, dev_exp, smooth_data, coef, Ftest_P )
  	
  	return(out)
}
