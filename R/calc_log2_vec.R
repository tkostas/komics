#' Calculate log2 from a vector
#'
#' @description Reads a numeric element or a vector and returns its log2 value. Output is numeric.
#' Missing values will be replaced with 0. You can use \code{convert_to_vec} or \code{convert_to_df}
#' to convert 0 back to NA.
#'
#' @param x Numeric element or vector for which the log2 value will be calculated.
#' @examples
#' x <- seq(from = 100, to = 1000, by = 100)
#' x
#' calc_log2_vec(x)
#' @export

calc_log2_vec <- function(x) {
  y <- log2(x + 1)
  return(as.numeric(y))
}

