get_single_col_index_df <- function(x, char) {
  # getting indexes for means
  print("Identifing columns with mean values ...")
  indx <- get_col_indexes_df(x, char)
  if (length(indx) == 1) {
    print(paste0("Column '", colnames(x)[indx], "' matched."))
  } else if (length(indx) == 0) {
    stop("No match found. Check input name and try again.", call. = FALSE)
  } else if (length(indx) > 1) {
    stop("More than one columns are assigned. Make character string more specific and try again.", call. = FALSE)
  }
  return(indx)
}
