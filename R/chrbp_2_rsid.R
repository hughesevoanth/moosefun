#' A Function that uses biomaRt to extract the rsid for provided chromosome and base pair.
#'
#' This function extracts rsids for provided mapping coordinates. Specific for build hg19.
#' @param chrbp chromosome:basepair character
#' @keywords biomart, rsid, map, annotation
#' @export
#' @examples
#' chrbp_2_rsid()
chrbp_2_rsid = function(chrbp){
  ## define the data base
  variation = useMart(host = "feb2014.archive.ensembl.org", 
                    biomart = "ENSEMBL_MART_SNP", 
                    dataset = "hsapiens_snp")
  ##
  cat( paste0("\nExtracting data from build hg19 via Ensembl archive Feb 2014.\n\n") )
  ##
  ## Screen biomaRt for rs
  out = t( sapply(chrbp, function(map){
    ## place the system to sleep for 1 second
    ## I have found that too many iterative requests
    ## to biomart causes an error
    Sys.sleep(1)
    ## extract chr and bp
    x = strsplit(map, split = ":")[[1]]
    chr = as.numeric(x[1])
    pos = as.numeric(x[2])
    ##
    o = getBM(attributes=c('refsnp_id','chr_name','chrom_start', 'allele',
      "minor_allele", "minor_allele_freq", 
      "allele_1", "ensembl_gene_stable_id" ), 
                 filters = c( 'chrom_start', 'chrom_end' ,'chr_name'),
                 values = list( start = pos, end = pos,  chr = chr),
                 mart = variation)
    return( unlist(o[1,]) )
    }) )
  ####
  out = data.frame(
    rsid =  unlist(out[,1]) ,
    chr = unlist(out[,2]),
    bp = unlist(out[,3]),
    alleles = unlist(out[,4]),
    minor_allele = unlist(out[,5]),
    MAF = unlist(out[,6]),
    ancestral_allele = unlist(out[,7]),
    ENSID = unlist(out[,8]), 
    stringsAsFactors = FALSE 
    )

  return(  out  )
}