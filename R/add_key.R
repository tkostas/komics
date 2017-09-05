#' Add key column in the begining of your dataframe.
#'
#' @description Add a column with key numbers in the begining of the dataframe.
#'            You can use this column later as a reference to join your dataset.
#'
#' @param x Input dataframe.
#' @param colname Custom name for the key column. Default value is "key".
#'
#' @examples
#'
#' x <- data.frame(V1 = c("A", "B", "C"),
#'                 V2 = c("D", "E", "F"))
#' add_key(x, "key_column")
#' @export

add_key <- function(x, colname = "key"){
  rows_df <- nrow(x)
  key_column <- seq(1:rows_df)
  key_column <- as.data.frame(key_column)
  colnames(key_column) <- colname
  output <- cbind(key_column, x)
  print("Key column added as first column in the dataframe.")
  return(output)
}

