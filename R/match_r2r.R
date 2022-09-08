#' Get phenotypes, crossing and genotypes and match them
#'
#' @param geno, genotype table
#' @param pheno, phenotype table
#' @param fun_codes, "funnel" codes
#'
#' @return a list with three elements: the genotype table, 
#' the phenotype table and the funnel codes for the common
#' individuals between the three tables.
#' 
#' @export
#'
#' @examples
#' # TODO: example
match_r2r <- function(geno, pheno, fun_codes) {
  # Individus communs
  
  aa <- sapply(strsplit(rownames(geno), "[.]"), "[[", 1)
  bb <- sapply(strsplit(pheno$Line, "[.]"), "[[", 1)
  cc <- sapply(strsplit(as.vector(fun_codes$id), "[.]"), "[[", 1)
  IDS <- Reduce(intersect, list(aa, bb))
  
  GENO <- matrix(nrow = 0, ncol = ncol(geno))
  PHENO <- matrix(nrow = 0, ncol = ncol(pheno))
  CROSS <- matrix(nrow = 0, ncol = ncol(fun_codes))
  
  for (i in 1:length(IDS)) {
    if (sum(aa == IDS[i]) & sum(cc == IDS[i]) & sum(cc == IDS[i])) {
      pos.geno <- which(aa == IDS[i])[1]
      pos.pheno <- which(bb == IDS[i])
      pos.cross <- which(cc == IDS[i])[1]
      
      G <- geno[rep(pos.geno, times = length(pos.pheno)), ]
      CR <- fun_codes[rep(pos.cross, times = length(pos.pheno)), ]
      P <- pheno[pos.pheno, ]
      
      GENO <- rbind(GENO, G)
      PHENO <- rbind(PHENO, P)
      CROSS <- rbind(CROSS, CR)
      
    }
    
  }
  
  colnames(GENO) <- colnames(geno)
  colnames(PHENO) <- colnames(pheno)
  colnames(CROSS) <- colnames(fun_codes)
  
  return(list(
    GENO = GENO,
    PHENO = PHENO,
    CROSS = CROSS
  ))
  
}

