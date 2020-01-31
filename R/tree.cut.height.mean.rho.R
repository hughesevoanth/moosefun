#' A Function to iterate over a variety of tree cut heights to estimate the number of clusters, the min, the mean, the max correlation estimate for all elements with a cluster.
#'
#' This function estimates the correlation structure among two factors, using a chi-square test and the Crammer's V statsitic
#' @param hclust_data an hclust object
#' @param CMat a correlation matrix used to generate the hclut object
#' @keywords correlation estimates within clusters, data reduction, variable independence
#' @export
#' @examples
#' tree.cut.height.mean.rho()
tree.cut.height.mean.rho = function(hclust_data, CMat){
	### a selection of tree cut heights 
	h = seq(0.1, 0.9, by = 0.01)
	###  estimate rho for each tree cut height
	Kcount_rho = t( 
		sapply(h, function(x){
		## tree cut at height x
		kout = cutree(hclust_data, h = x)
		## the number of k groups
		l = length( table(kout) )
		## the number of k groups with an n > 1
		L = sum(table(kout) > 1 )
		### Average cluster size
		avg_cluster_size = mean(table(kout))
		### average non_zero Cluster size
		w = which(table(kout) > 1)
		avg_non1_cluster_size = mean(table(kout)[w])


		### now iterate over the k clusters that have an n > 1
		### and estimate the mean, min, and max, spearman's rho
		
		## the unique cluster numbers of clusters with more than 1 metabolite
		Ks = which(table(kout) > 1)

		## a function to estimate mean, min and max
		k_means = t(
		 sapply( Ks, function(k){
			n = names( which(kout == k) )
			######
			temp_D = CMat[n,n]
			temp_D = temp_D[lower.tri(temp_D)]
			######
			min_mean_max_spear = c( min(abs(temp_D), na.rm = TRUE) , 
					mean( abs(temp_D), na.rm = TRUE),
					max( abs(temp_D), na.rm = TRUE)  )
			
		return(na.omit( min_mean_max_spear) )
		
		})
		 )
		####
		colnames(k_means) = c("min","mean","max")

		### estimate the average min, mean, and max estimates
		mean_of_means = apply(k_means, 2, function(x){mean(x, na.rm = TRUE)})
		
		### vector to return to super fucntion
		out = c(x, l , L, avg_cluster_size, avg_non1_cluster_size, mean_of_means)
		names(out) = c("cut_height", "n_groups",
			"n_groups_greaterthan1",
			"avg_cluster_size",
			"avg_non1_cluster_size",
			"min_of_means_rho_of_groups",
			"mean_of_means_rho_of_groups",
			"max_of_means_rho_of_groups")
		return(out)
		})
	)
	
	return(Kcount_rho)

}

