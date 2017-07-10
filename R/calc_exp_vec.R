#' Calculate exponential base^x from a vector
#' @description Calculate exponential basex. Reads a numeric
#' element convert to the power specified.
#'
#' @param x Numeric vector.
#' @param base Numeric element that you will raise \code{x}.
#'
#' @examples
#' x <- 4
#' base <- 2
#' calc_exp(a, x)
#'
#' v <- seq(1:5)
#' calc_exp(v)
#' @export

calc_exp_vec <- function(x, base = 2) {
  y <- base^x
  return(as.numeric(y))
}
