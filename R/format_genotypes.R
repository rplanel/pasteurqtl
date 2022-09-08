#' Transform genotype format to R/qtl2's
#'
#' @param geno_ag_KC, genotype table
#'
#' @return the recoded genotypes
#' @export
#'
#' @examples
#' # TODO: example
format_genotypes <- function(geno_ag_KC) {
  lv = c("A", "C", "G", "T", "H", "N")
  
  for (i in 1:ncol(geno_ag_KC)) {
    locus <- factor(geno_ag_KC[, i], levels = lv)
    freqs <- table(locus)
    
    n.all <- length(lv[freqs > 0 & lv != "H" & lv != "N"])
    
    if (n.all == 2) {
      geno_ag_KC[, i][geno_ag_KC[, i] == lv[freqs > 0][1]] <- "A"
      geno_ag_KC[, i][geno_ag_KC[, i] == lv[freqs > 0][2]] <- "B"
      geno_ag_KC[, i][geno_ag_KC[, i] == "H"] <- "N"
      
    } else{
      geno_ag_KC[, i] <- "N"
    }
    
  }
  
  return(geno_ag_KC)
  
}
