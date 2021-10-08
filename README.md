# Random useful functions for Moose's work

### Authors: David Hughes 
##### Date started: 31st Jan. 2020

### About:

This repo contains a variety of helpful functions that I come back to time and time again.  Rather than adding them into a variety of different packages I will try to maintain this catch-all that I and my colleagues can source when necessary. 

### Installation instructions of the moosefun package

	1. insure that the devtools library is installed on your local machine
		 > ifelse("devtools" %in% rownames(installed.packages()), 
		 NA, 
		 install.packages("devtools"))
		 
	2. install moosefun
		> devtools::install_github("hughesevoanth/moosefun")

___
		
## iPVs functions have moved to their own package 

	Date moved: March 6th 2020
	For functions to aid in the identification of principal variables in inter-correlated data sets please follow the path below.
	
	https://github.com/hughesevoanth/iPVs

---
## coverting rsid to SNPID and SNPID to rsid

### the useful functions are: rsid_2_chrbp() & chrbp_2_rsid()

- Do note that this function is only good for a handful (1 to a few hundred) of SNPs that need an id conversion. 
- The reason for this is because for each ID to convert a query is sent over the web and a built in sleep step needed to be implemented to avoid query crashes, which can still occur. 
- If you can I would avoid trying to run these functions for more than ~100 SNPs at a time. 
- If you need to convert thousands of IDs look into using ANNOVAR (https://annovar.openbioinformatics.org/en/latest/) or some other tool. 

EXAMPLE:

	#########################
	## install biomaRt
	#########################
	if (!requireNamespace("BiocManager", quietly = TRUE))
	    install.packages("BiocManager")
	BiocManager::install("biomaRt")
	
	#########################
	## 	load libraries
	#########################
	library(moosefun)
	library(biomaRt)

	#########################
	## convert rsids to snpids
	#########################
	## read in the SNP list
	snps = c("rs4987657","rs4987667","rs4987682")
	## Extract mapping cooridinates
	map = rsid_2_chrbp(snps)
	o = order(map$chr, map$bp)
	map = map[o,]
	
	#########################
	## convert snpids to rsids
	#########################
	## read in the SNP list
	snps = c("7:142569596:A_G","7:142572908:T_C","7:142574913:A_G")

	## NOTE: the function splits snpids on ":" and only uses the first two 
	## strings as chr and bp, all else is ignored
	
	## Extract mapping cooridinates
	map2 = chrbp_2_rsid(snps)
	o = order(map2$chr, map2$bp)
	map2 = map2[o,]


-- 
## DESCRIPTION OF OTHER FUNCTIONS


### ztransform()
- taken from the GenABEL package to perform z-transformations. This function is necessary to run my edited version of the rank normal transformation function.

### rntransform()
- taken from the GenABEL package to perform rank normal transformations, but **edited to randomly split tied values**. 

### moose_biplot()
-  a function to plot a biplot or a PCA with a loadings plot on top of it. 
-  however, the uniqueness here is that we are not plotting the loading from variables used in the construction of the PCA. 
- rather, we are passing a novel set of variables | traits | phenotypes that will be correlated to the PC-axis (1 and 2) and then plotted. 
- as an example:
	1. generate a prcomp object:
		
			pca = prcomp( iris[, 1:4] )
	
	2. or you can generate a probabilistic pca (pcaRes) object
		
			pca = ppca( as.matrix( iris[, 1:4] ), nPCs = 4)
	
	3. run the function
		
			moose_biplot(PCA = pca, dataframe_of_phenotypes = iris[, 1:4], 
             plot_top_N_phenotypes = 3, 
             grouping1 = iris$Species, grouping1NAME = "species",
             grouping2 = iris$Species, grouping2NAME =  "species",
             scalearrows = FALSE )

	- the dataframe_of_phenotypes can be any matrix of quantitative trait with the same number of row as passed to the prcomp() or ppca() functions.
	- *grouping1* dictates the color scheme and the ellipses to be drawn **Currently limited to 9 groups**
	- *grouping2* dictates the plot shapes to be used. **Currently limited to 5 groups**
	- *scalearrows* allows you to scale the largest correlated trait to a rho of 1, and all other arrows in correspoinding manner. This can be done to aid in visualization. Note:  if *scalearrows* is set to TRUE, the relative length of the arrows remain informative.
	

