#' Calculate fold change
#' @description Reads a data frame, identifies the columns containing group mean values, and
#' calculates the log2 fold difference. Output is saved in a new column of the same data frame
#' labeled as “log2_fold_change_AB”. If means are not calculated, used \code{calc_means_df} function first.
#' @param x Data frame containing input values.
#' @param mean_A Character string describing the column name of the mean for the
#' group A. Partial matching is acceptable.
#' @param mean_B Character string describing the column name of the mean for the
#' group B. Partial matching is acceptable.
#'
#' @examples
#' proc_data <- calc_fold_change_df(data, "TIC_Pat", "TIC_Ctr")
#' @export
#'
calc_fold_change_df <- function(x, mean_A, mean_B) {
  print("---------------------")
  print(paste("Calculating fold change for group A", mean_A, "vs group B", mean_B))
  print("---------------------")
  # getting indexes for means
  print("Identifing columns with mean values ...")
  indx_A <- get_col_indexes_df(x, mean_A)
  if (length(indx_A) == 1) {
    print(paste0("Column '", colnames(x)[indx_A], "' matched for group A."))
  } else if (length(indx_A) == 0) {
    stop("No match found for group A. Check input name and try again.", call. = FALSE)
  } else if (length(indx_A) > 1) {
    stop("More than one columns are assigned to group A. Make character string more specific and try again.", call. = FALSE)
  }
  indx_B <- get_col_indexes_df(x, mean_B)
  if (length(indx_B) == 1) {
    print(paste0("Column '", colnames(x)[indx_B], "' matched for group B."))
  } else if (length(indx_B) == 0) {
    stop("No match found for group B. Check input name and try again.", call. = FALSE)
  } else if (length(indx_B) > 1) {
    stop("More than one columns are assigned to group B. Make character string more specific and try again.", call. = FALSE)
  }
  # calculate fold change
  print("Calculating fold change ...")
  fold_change <- x[,indx_A]/x[,indx_B]
  log2_fold <- log(fold_change, base = 2)
  x$log2_fold_change_AB <- log2_fold
  print("Done!")
  return(x)
}
