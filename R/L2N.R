#' Transform letters into numbers 
#'
#' @param x, string
#'
#' @return characters
#' @export
#'
#' @examples
#' L2N("N")
#' sapply(c("A", "C", "G"), L2N)
#' 
L2N <- function(x) {
  match(x, LETTERS)
}
