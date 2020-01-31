#' A Function to describe each feature in a data frame of metabolite data. 
#'
#' This function estimates the correlation structure among two factors, using a chi-square test and the Crammer's V statsitic
#' @param PCA a prcomp object
#' @param dataframe_of_phenotypes a dataframe of phenotypes to correlate against PC1 and PC2 of the prcomp object. The function currently only uses quantitative traits in the analysis. Categorical is not possible, yet. 
#' @param plot_top_N_phenotypes an integer specifying how many of the top correlated phenotypes should be plotted
#' @param groupfactor a vector of class categorical, of the sample length, as the number fo data points in PC1 and PC2. This categorical data will aid in color coding your plotted points. 
#' @keywords biplot
#' @export
#' @examples
#' moose_biplot()
moose_biplot = function(PCA, dataframe_of_phenotypes, plot_top_N_phenotypes = 3, groupfactor){
	
	##############################
	## build correlation matrix
	## pf PCs against external variables
	##############################
	varexp = summary(PCA)[[6]][2,]

	mydata = data.frame( PC1 = PCA$x[,1], PC2 = PCA$x[,2], class = groupfactor)

	## empty Correlatiom Matrix
	cormat = matrix(NA, ncol(dataframe_of_phenotypes) , 4, 
		dimnames = list( colnames(dataframe_of_phenotypes), paste0("PC", 1:4) ) )
	## estimate spearman
	for(i in 1:ncol(dataframe_of_phenotypes)){
		for(j in 1:4){
			est = 	cor.test(dataframe_of_phenotypes[,i], PCA$x[,j], method = "spearman")$estimate
			cormat[i,j] = est
		}
	}

	##  what are the top correlated phenotypes for PC1 and PC2
	##  I define this as the sum of the spearm correlations
	rhosum = apply(abs(cormat[, 1:2]), 1, sum)
	k = order(rhosum, decreasing = TRUE)[1:plot_top_N_phenotypes]
	cormat = cormat[k, ]

	## redefine the rotation | loadings in the PCA
	PCA[[2]] = cormat

	## stealing  code from ggbiplot
	pcobj = PCA

	nobs.factor <- sqrt(nrow(pcobj$x) - 1)
	d <- pcobj$sdev
	u <- sweep(pcobj$x, 2, 1/(d * nobs.factor), FUN = "*")
	v <- pcobj$rotation

	choices = 1:2
	obs.scale = 0
	var.scale = 1
	#circle.prob = 0.69
	circle.prob = 0.8
	df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale, FUN = "*"))
	#v <- sweep(v, 2, d^var.scale, FUN = "*")
	df.v <- as.data.frame(v[, choices])
	names(df.u) <- c("xvar", "yvar")
	names(df.v) <- names(df.u)

	df.u <- df.u * nobs.factor

	r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
	v.scale <- rowSums(v^2)
	df.v <- r * df.v/sqrt(max(v.scale))
	df.v$labels = rownames(df.v)   


	### GGPLOT 
	plotout = mydata %>% ggplot( aes(x = PC1, y = PC2, color = class) ) +
	geom_point(size = 5) + 
	geom_text(data = df.v, aes(label = labels, 
		x = xvar, y = yvar), nudge_x = 0.35, 
	color = "black", size = 5) +
	geom_segment(data = df.v, aes(x = 0, y = 0, xend = xvar, yend = yvar), 
		arrow = arrow(length = unit(1/2, "picas")), 
		size = 1, color = "grey50") +
	labs(x = paste0("PC1;  variance explained = ", signif(varexp[1], d = 4) ),
		y = paste0("PC2;  variance explained = ", signif(varexp[2], d = 4) ) )
	
	return(plotout)


}
