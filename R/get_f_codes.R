#' Read crossing tables et generate random data to replace missing data
#'
#' @param file_f_codes, path to crossing data
#'
#' @return funnel codes
#' @export
#'
#' @examples
#' # TODO: example
#' @importFrom utils read.csv
get_f_codes <- function(file_f_codes) {
  ## Ordre de croisement
  fun_codes <- read.csv(file_f_codes)
  rownames(fun_codes) <- fun_codes$id
  
  ### Genere des codes de croisement aleatoires (beurk)
  n_rep <- sum(is.na(fun_codes$Funnel_Code))  # quite dirty
  dummy_code <- character(0)  # quite dirty
  for (i in 1:n_rep) {
    dummy_code[i] <-
      paste(sample(LETTERS[1:8], 8), collapse = "")  # quite dirty
  }
  tmp_vect <- as.vector(fun_codes[, 2]) # quite dirty
  tmp_vect[is.na(fun_codes$Funnel_Code)] <- dummy_code # quite dirty
  fun_codes$Funnel_Code <- as.factor(tmp_vect) # quite dirty
  
  return(fun_codes)
}

