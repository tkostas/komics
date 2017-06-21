#' Calculate CV for one vector
#' @description Reads a vector and calculates the CV value
#'
#' @param x Numeric vector.
#' @examples
#' x <- seq(1:10)
#' calc_CV_vec(x)
#' @export
#'
calc_CV_vec <- function(x) {
  cv <- sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE) * 100
  return(as.numeric(cv))
}


