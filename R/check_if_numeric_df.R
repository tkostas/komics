

check_if_numeric_df <- function(x) {
  # check if values are numeric
  print("Checking if values are numeric ...")
  i <- 1
  for (i in seq_along(colnames(x))) {
    if (!is.numeric(x[,i])) {
      stop(paste0("Column '", colnames(x[i]), "' should be numeric. Check if there is any text."))
    }
  }
  print("All columns are numeric.")
}
