#' Append extra columns to a dataframe
#' @description Reads two data frames \code{x, y} and based on the user defined key
#' columns \code{key_x, key_y}, add the columns described by \code{append_y} to the \code{x} table.
#'
#' @param x Main working data frame. \code{NA} will be added in the rows on \code{x} that do not match with \code{y}.
#' @param y Secondary data frame containing columns to be added in the main one. Columns that do not match with x will be excluded.
#' @param key_x Character string describing key column of the first data frame. You can use partial matching.
#' @param key_y Character string describing key column of the second data frame. You can use partial matching.
#' @param append_y Character vector containing strings that match the columns to be added in the main data frame. You can use also partial matching.
#'
#' @examples add_me <- colnames(y)[c(4, 10)]
#' ap <- append_cols_df(x = k3,
#'                   y = y,
#'                   key_x = "Protein A",
#'                   key_y = "Entry$",
#'                   append_y = add_me)
#' @export


append_cols_df <- function(x, y, key_x, key_y, append_y){
  print("---------------------")
  print(paste("Appending new columns ..."))
  print("---------------------")
  ## find key columns
  # find key in x
  print("Geting index for group A")
  indx_x <- get_single_col_index_df(x, key_x)
  by_x <- colnames(x)[indx_x]
  # find key in y
  print("Getting index for group B")
  indx_y <- get_single_col_index_df(y, key_y)
  by_y <- colnames(y)[indx_y]
  # find indexes for new columns
  indx_app <- get_col_indexes(y, append_y)
  if (length(indx_app) == 0) {
    stop("No columns for appending found in the 2nd table. Check character string and try again.", call. = FALSE)
  } else {
    print(paste(length(indx_app), "columns from the 2nd table will be added ..."))
    app_y <- colnames(y)[indx_app]
  }
  output <- merge(x = x, y = y[,c(by_y, app_y)], by.x = by_x, by.y = by_y, all.x = TRUE )
  before_n <- dim(x)[1]
  before_p <- dim(x)[2]
  after_n <- dim(output)[1]
  after_p <- dim(output)[2]
  print("Done!")
  print(paste("Initial table contained", before_n, "rows and ", before_p, "columns." ))
  print(paste("Output contains", after_n, "rows and ", after_p, "columns." ))
  return(output)
}
