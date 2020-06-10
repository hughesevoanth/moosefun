#' A Function that uses biomaRt to extract chromosome and base pair for your rsid
#'
#' This function extracts mapping coordinates for your rsid
#' @param rsid dbSNP reference id
#' @keywords biomart, rsid, map, annotation
#' @export
#' @examples
#' rsid_2_chrbp()
rsid_2_chrbp = function(rsid){
  ## define the data base
  variation = useMart(host = "feb2014.archive.ensembl.org", 
                    biomart = "ENSEMBL_MART_SNP", 
                    dataset = "hsapiens_snp")
  ##
  cat( paste0("\nExtracting data from build hg19 via Ensembl archive Feb 2014.\n\n") )
  ##
  ## Screen biomaRt for rs
  out = t( sapply(rsid, function(snp){
    ## place the system to sleep for 1 second
    ## I have found that too many iterative requests
    ## to biomart causes an error
    Sys.sleep(1)
    ##
    o = getBM(attributes=c('refsnp_id','chr_name','chrom_start', 'allele', 
      "minor_allele", "minor_allele_freq", 
      "allele_1", "ensembl_gene_stable_id"), 
                 filters = c( 'snp_filter'),
                 values = rsid,
                 mart = variation)
    return( unlist( o[1,] ) )
    }) )
  #### convert to a data.frame
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
  #### return data
  return(out)
}
