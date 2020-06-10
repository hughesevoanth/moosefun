#' A Function that uses biomaRt to annotate SNPIDs
#'
#' This function extracts mapping and (+/- 150KB ) gene annotation information for SNPids
#' @param snpid chr:basepair_A1_A2
#' @param plusminus_dist plus minus distsance in kilobases (kb) to annotate nearby genes.
#' @keywords biomart, rsid, map, annotation
#' @export
#' @examples
#' snpid_2_geneanno()
snpid_2_geneanno = function(snpid, plusminus_dist = 150){
  ## define the data base
  genes = useMart(host = "feb2014.archive.ensembl.org", 
                biomart = "ENSEMBL_MART_ENSEMBL", 
                dataset = "hsapiens_gene_ensembl")
  ##
  cat( paste0("\nExtracting data from build hg19 via Ensembl archive Feb 2014.\n\n") )
  ##
  snpid = as.character(snpid)
  # print(snpid)
  temp = strsplit(snpid, split = ":")[[1]]
  chr = as.numeric( temp[1] )
  pos = as.numeric( strsplit(temp[2], split = "_" )[[1]][1] )
  
  ##
  pm = plusminus_dist * 1000
  VAL = list(chr, (pos - pm), (pos + pm) )
  
  out <- getBM(attributes=c('ensembl_gene_id','external_gene_id',
                            'start_position','end_position',
                            'gene_biotype','description',
                            'go_id','name_1006',
                            'definition_1006',
                          'go_linkage_type', 'namespace_1003',
                          'goslim_goa_accession', 
                          'goslim_goa_description'), 
               filters = c('chromosome_name','start', 'end' ),
               values = VAL,
               mart = genes)
  if(nrow(out)>0){
    ###
    ENSgenesPlusMinus150K = paste0(unique(out[,1]), collapse = ":")
    genesPlusMinus150K = paste0(unique(out[,2]), collapse = ":")
    genesbiotype = paste0(unique(out[,5]), collapse = ":")
    genesDescription = paste0(unique(out[,6]), collapse = ":")
    ###################
    ## closest gene:
    ###################
    ## ABS distance
    d1 = abs( out[,3] - pos )
    d2 = abs( out[,4] - pos )
    m = min(c(d1,d2))  
    w = c(which(d1 == m), which(d2 == m))
    temp = out[w,]
    ENSclosest = unique(temp[,1])
    GIDclosest = unique(temp[,2])
    Gstart = temp[1,3]
    Gend = temp[1,4]
    Gdes = temp[1,6]
    GOSLIMid = paste0(unique(temp[,12]), collapse = ";")
    GOSLIMidNAME = paste0(unique(temp[,13]), collapse = ";")
    ###
    DATAOUT = c(ENSclosest, GIDclosest, Gstart, Gend, 
                Gdes, GOSLIMid, GOSLIMidNAME,
                ENSgenesPlusMinus150K, 
                genesPlusMinus150K, 
                genesbiotype, 
                genesDescription)
  } else {
    DATAOUT = rep(NA, 11)
  }
  return(DATAOUT)
}
