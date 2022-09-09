#!/usr/bin/env Rscript

## ----setup, include=FALSE----------------------------------------
## Il faudra sans doute paralléliser (à discuter)

library(qtl2)

args <- commandArgs(trailingOnly = TRUE)
nrep <- args[1] ## possible values : an integer
crossdata_path <- args[2]
clean_pr_path <- args[3]
cores <- args[4]

load(crossdata_path)
load(clean_pr_path)
dir.create("outfiles/Rdata", recursive = TRUE)


# To allow unequal number of individuals per CC line
cc_lines = substring(rownames(crossdata$pheno), 1, 5) # note : to do so, rownames of cross$pheno abs need to satisfy specific encoding, eg B6x11.5 where B6 and 11 are cross parents, dot the separator, and mouse idenitfyier in that block is 5. The 5-character before sep can both fit CCxRIX or CC-F1s (cf example) and CC tout court, eg CC011.5 (using 3digit identifyer for CC mice, and shortened to two for F1s)
indices_all = c(1:length(cc_lines))

indices = tapply(indices_all, cc_lines, min)
nbs = tapply(indices_all, cc_lines, length)

#et ensuite rm ce qui n'est plus utile
rm(cc_lines)
rm(indices_all)


lod_perm_all <- numeric(nrep)

for (j in 1:nrep) {
  i <- rep(sample(indices), times = nbs)
  pr_perm <- lapply(clean_pr, function(A) {
    B <- A[i, , ]
    dimnames(B) <- dimnames(A)
    return(B)
  })

  attributes(pr_perm) <- attributes(clean_pr)
  class(pr_perm) <- class(clean_pr)
  apr_perm <- genoprob_to_alleleprob(pr_perm, cores = cores)
  kloco_perm <- calc_kinship(apr_perm,  type = "loco", use_allele_probs = TRUE, omit_x = TRUE, cores = cores) #redo kloco given perm
  out_perm <- scan1(pr_perm, crossdata$pheno, kinship = kloco_perm, cores = cores) # genome scan using linear mixed models
  #  lod_perm_all[j] <- maxlod(out_perm)

  if (j == 1) {
    lod_perm_per_var <- apply(out_perm, 2, FUN = maxlod)
  } else {
    lod_perm_per_var <- rbind(
      lod_perm_per_var,
      apply(out_perm, 2, FUN = maxlod)
    )
  }
}



## save results
# write.csv(data.frame(significance = names(Q), threshold = unname(Q)), "significance-threshold.csv",row.names = FALSE)

# write.csv(peaks, "peaks.csv")

## save(lod_perm_all, file = "lod_perm_all.dat")

save(lod_perm_per_var, file = "outfiles/Rdata/lod_perm_per_var.dat")
save(nrep, file = "outfiles/Rdata/nrep.dat")