#' Get genotype files (founders and individuals) with identical loci
#'
#' @param file_founder, string, path to founder file
#' @param file_individuals, string, path to individual files
#'
#' @return list of genotypes and loci
#' @export
#'
#' @examples
#' # TODO: example
homogeneize_gen <- function(file_founder, file_individuals) {
  geno_tmp <- read.csv(file_individuals)
  geno <- t(as.matrix(geno_tmp[,-(1:3)]))
  colnames(geno) <- geno_tmp$marker
  geno_tmp$marker <- colnames(geno)
  
  geno_founders_tmp <- read.csv(file_founder)
  geno_founders <- t(as.matrix(geno_founders_tmp[,-(1:3)]))
  colnames(geno_founders) <- geno_founders_tmp$marker
  geno_founders_tmp$marker <- colnames(geno_founders)
  
  l.ids <- intersect(geno_founders_tmp$marker, geno_tmp$marker)
  geno_founders <- geno_founders[, l.ids]
  geno <- geno[, l.ids]
  
  ## Carte physique
  map_ag <- geno_tmp[, 1:3]
  rownames(map_ag) <- map_ag$marker
  map_ag <- map_ag[l.ids, ] 
  print(str(map_ag))
  print(head(map_ag))
  p_map <- map_ag
  map_ag[, 3] <- map_ag[, 3] / 1939394
  p_map[, 3] <-  p_map[, 3] / 1000000

  list(
    gen_ind = geno,
    gen_founders = geno_founders,
    genetic_map = map_ag,
    locus_list = l.ids,
    physical_map = p_map
  )
  
}