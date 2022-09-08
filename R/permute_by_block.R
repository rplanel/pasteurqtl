#' Permute by block; TOFIX
#'
#' @param f 
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#' 
permute_by_block <- function(f, x) {
  n <- nrow(x)
  i <- rep(sample(sapply(split(1:n, f), "[", 1)), table(f))
  z <- x[i,]
  
  return(z)
}