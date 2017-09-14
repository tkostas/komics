#' Remove columns from a dataframe
#' @description Use partial matching to select and remove multiple columns from a dataframe.
#'
#' @param x A dataframe.
#' @param columns A vector containing part of the column names that are to be removed.
#'         Partial matching of the character string will be used. You can use normal
#'         regular expressions to describe the name.
#'
#' @examples
#' data <- data.frame(L1 = c("a", "b", "c", "d", "e", "f", "g", "h"),
#'                   V1 = c(1, 2, 3, 6, 1, 3, 4, 4),
#'                   V2 = c(3, 4, 5, 3, 5, 5, 3, 2),
#'                   Z3 = c(6, 7, 4, 2, 3, 2, 7, 6))
#'  data_trim <- drop_columns(data, "V")
#' @export


drop_columns <- function(x, columns) {
  column_indexes <- komics::get_col_indexes(x, columns)
  x[,column_indexes] <- NULL
  print(paste("Ok,", length(column_indexes), "columns were removed."))
  return(x)
}
