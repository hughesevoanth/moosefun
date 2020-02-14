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
moose_biplot = function(PCA, dataframe_of_phenotypes, plot_top_N_phenotypes = 3, 
                        grouping1 = NA, grouping1NAME = NA,
                        grouping2 = NA, grouping2NAME = NA, cir.size = 1.75, 
                        scalearrows = FALSE){
	######################################
  ## extract needed data from the PCA
  ######################################
  if( class(PCA)[1] == "prcomp"){
    ## variance explain
    varexp = summary(PCA)[[6]][2,]
      wdata = data.frame( PC1 = PCA$x[,1], PC2 = PCA$x[,2], class = grouping1, class2 = grouping2)
	} else {
	  if( class(PCA) == "pcaRes"  ){
	    varexp =  summary(PCA)[1,] 
	    wdata = data.frame( PC1 = PCA@scores[,1], PC2 = PCA@scores[,2], class = grouping1, class2 = grouping2)
	  }  else {
	    
	    stop( "The PCA object you pass to moose_biplot must be from prcomp() or ppca()." ) 
	      
	    }
	}
  
  ######################################
  ##  insure that the PCA eigenvectors
  ##  are zero centered
  ##
  ######################################
  wdata[, 1:2] = apply(wdata[, 1:2], 2, function(x){  x - mean(x, na.rm = TRUE)  })
  
  
  ######################################
  ## Build the correlation matrix
  ## between the PCs and the matrix
  ## of phenotypes
  ######################################
  ## make sure that the sample size of the PCA and the phenotype data set are equal.
  ## the method does assume that the samples are aligned and ordered. 
  
  if( nrow(wdata) != nrow(dataframe_of_phenotypes) ){
     stop(  "The number of sapmles in your PCA do not match the number of samples in your dataframe_of_phenotypes"  )
    }
  
  ##############################
	## empty Correlatiom Matrix
  ##############################
  cormat = matrix(NA, ncol(dataframe_of_phenotypes) , 2, 
		dimnames = list( colnames(dataframe_of_phenotypes), paste0("PC", 1:2) ) )
	
  ##############################
	## estimate Spearman's rho
  ##############################
	for(i in 1:ncol(dataframe_of_phenotypes)){
		for(j in 1:2){
			est = 	suppressWarnings( cor.test(dataframe_of_phenotypes[,i], wdata[,j], method = "spearman")$estimate )
			cormat[i,j] = est
		}
	}

	##  what are the top correlated phenotypes for PC1 and PC2
	##  (1) defined this as the sum of the spearm correlations
	rhosum = apply(abs(cormat[, 1:2]), 1, sum)
	p3 = order(rhosum, decreasing = TRUE)
  
	##  (2) ordering PC1 and then PC2; #extract most positive and most negative
	p1 = order( abs(cormat[,1] ), decreasing  = TRUE)#[ c(1, nrow(cormat) )]
	p2 = order( abs(cormat[,2] ), decreasing  = TRUE)#[ c(1, nrow(cormat) )]
	
	## extracting the variables to plot
	o = unique( c(p1[1], p2[1], p3[1]) )
	#o = unique( c( p1, p2, p3 ) )
	
	## what loadings|correlations to plot in the loadings plot
	new_cormat = cormat[o, ]
  
	
	## scale the arrow size to make them longer on the plot
	if(scalearrows == TRUE){
	  new_cormat = new_cormat / max( abs(new_cormat) )
	}
	
	
	###############################################
	## scaling the correlations to match the range 
	##  of data in the PCA plot
	###############################################
	pc1_scale = min( abs( range(wdata[,1]) ) ) * 0.95
	pc2_scale = min( abs( range(wdata[,2]) ) ) * 0.95
	
	new_cormat[,1] = new_cormat[,1] * pc1_scale
	new_cormat[,2] = new_cormat[,2] * pc2_scale
	new_cormat = as.data.frame(new_cormat)
	new_cormat$labels = rownames(new_cormat)
	
	#############################
	## Identify the center point
	## of each Grouping1
	#############################
	
	## unique levels
	u = sort( as.character( unique(wdata$class) ) )
	## redefine order of levels
	wdata$class = factor(wdata$class, levels = u)
	## estimate center
	centerpoint = as.data.frame( t( sapply(u, function(x){
	  w = which(wdata$class == x)
	  out = apply(wdata[w, 1:2], 2, function(z){ mean(z, na.rm = TRUE)  })
	  }) ) )
	
	
	#############################
	##  Estiamte Ellipse
	##  for each class
	#############################
	theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
	circle <- cbind(cos(theta), sin(theta))
	######
	
	##
	edata = lapply(u, function(x){
	  w = which(wdata$class == x)
	  sigma = var(wdata[w,1:2])
	  mu = colMeans(wdata[w, 1:2] )
	  C = circle %*% chol(sigma) * cir.size
	  ###
	  out = t( apply(C, 1, function(x){x + mu}) )
	  out = data.frame(out, class = x)
	  return(out)
	  })
	###
	eldata = c()
	for(i in 1:length(edata)){ eldata = rbind(eldata, edata[[i]])  }
	

	
	
  ####################
	### GGPLOT 
	####################
	
	## plotting colors
	n = length( unique(wdata$class) )
	pcol = brewer.pal(n, "Set1")
	pcol2 = darken(pcol, 0.4)
	
	## plotting shapes
	n = length( unique(wdata$class2) )
	ppch = c(21,22,23,24,25)[1:n]
	
	## plot
	plotout = wdata %>% ggplot( aes(x = PC1, y = PC2 ) ) +
	    #geom_point(size = 5, shape = 21, aes( fill = class ) ) +
	    geom_point(size = 5,  aes( shape = class2, fill = class ), alpha = 1 ) + 
	    scale_fill_manual( values = pcol ) +
	    scale_shape_manual( values = ppch ) +
	    geom_point( data  = centerpoint, aes( x = PC1, y = PC2  ), 
	                size = 9, shape = 22, 
	                fill = pcol2, color = "black", stroke = 2) +
	    geom_path(data = eldata, aes(color = class), size = 1.5) +
	    scale_color_manual( values = pcol ) +
	    geom_segment(data = new_cormat, 
	                 aes(x = 0, y = 0, xend = PC1, yend = PC2), 
	                 arrow = arrow(length = unit(1/2, "picas")), 
	                 size = 1, color = "red") +
	    geom_text(data = new_cormat, aes(label = labels, x = PC1, y = PC2), 
	            nudge_x = 0.25, 
	            color = "black",
	            size = 5) +
	    labs(x = paste0("PC1;  variance explained = ", signif(varexp[1], d = 2), "%" ),
	         y = paste0("PC2;  variance explained = ", signif(varexp[2], d = 2), "%" ),
	         fill = grouping1NAME,
	         shape = grouping2NAME) +
	    guides(color=FALSE)
	
	## return the ggplot
	return(plotout)


}
