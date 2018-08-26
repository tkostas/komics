#' Above_threshold_vec
#'
#' @description Reads a numeric vector and calculates how many elements are above a given threshold.
#' Output is a single element numeric vector with the number of observations.
#'
#' @param x A numeric vector.
#' @param y Threshold with default value 0.
#'
#' @examples
#' x <- seq(1:10)
#' y <- 5
#' above_threshold_vec(x, y)
#' @export


above_threshold_vec <- function(x, y = 0) {
  l <- length(x[x > y])
  print(l)
  return(as.numeric(l))
}
