#' A Function to to perform a hypergeometric test and a fisher exact test
#'
#' This function estimates the correlation structure among two factors, using a chi-square test and the Crammer's V statsitic
#' @param allfeatures a vector of class categorical, for all elements tested, identifying a category | or bin | box that each element belong to
#' @param selectedfeatures a vector of class categorical, for all elements that are unique for some reason, identifying a category | or bin | box that each element belong to
#' @keywords hypergeometric test; fisher exact test
#' @export
#' @examples
#' hgtest()
hgtest = function(allfeatures, selectedfeatures){
	## unique list of categories in the selected feature list
	cats = na.omit( unique(selectedfeatures) )
	## iterate over those cat and perform test
	
	HG_F_test = sapply(cats, function(i){
		g = length(allfeatures)
		## total number of tested features with some annotation
		N = length(allfeatures) - ( sum(is.na(allfeatures) | allfeatures == "") )
		## number of features in cat i
		m = sum(allfeatures == i, na.rm = TRUE ) 
		## number of features NOT in cat i
		n = N - m
		## number of features selected by X criteria, that have some annotation
		k = length(selectedfeatures) - ( sum(is.na(selectedfeatures) | selectedfeatures == "") )
		## number fo selected features in cat i
		x = sum(selectedfeatures == i , na.rm = TRUE) 

		
		## estiamte fold enrichment and p.value
		fold.enrichment <-  (x / k ) / (m / N)
		p.value <-  phyper(q=x -1, m=m, n=n, k=k, lower.tail=FALSE)

		## FISHER EXACT TEST
		## BUILD 2 x 2 contigency table
		dmat = matrix( c(x,k-x,m-x,n-(k-x)),2,2, byrow  = TRUE, dimnames = list(c("Selected","NotSelected"), c("FocusedCat","AllOtherCats") )) 
		ftest <- fisher.test(x=dmat, alternative="greater")


		## data out
		out = c(fold.enrichment, p.value, ftest$estimate, ftest$p.value); names(out) = c("fold.enrichment", "HG_pval", "Ftest_OR", "Ftest_pval")
		return(out)
		})
	## return to user
	return(t(HG_F_test))

		

} 
