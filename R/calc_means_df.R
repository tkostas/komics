#' Calculate means
#' @description Calculate mean values for the specified columns of a dataframe. Output is appended
#' into the initial dataframe.
#' @param x Data frame with the input values.
#' @param groups 	List containing column identifiers for the different groups. The name of each
#' element will be used as column name containing the mean. Each element is a vector containing
#' possible identifiers for the columns. Identifiers can match partially with the columns of interest.
#'
#' @examples
#' groups <- list(mean_A = "group A",
#'                mean_B = c("^B", “groupB”)
#' data <- calc_means_df(x = input, groups = groups)
#' @export


calc_means_df <- function(x, groups) {
  output <- x
  for (i in seq_along(groups)) {
    print(paste("Getting indexes for ", groups[i]))
    indx <- get_col_indexes_df(x = x, id = groups[[i]])
    vec <- apply(x[,indx], 1, mean, na.rm = TRUE)
    output <- cbind(output, vec)
    vec_name <- paste(names(groups[i]))
    last_element <- length(colnames(output))
    colnames(output)[last_element] <- vec_name
  }
  print("Done!")
  return(output)
}
