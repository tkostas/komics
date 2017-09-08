#' Move columns in a dataframe
#'
#' @description Re-arrange a dataframe by moving columns. You can use partial match to
#'              select the columns of interest (See \code{\link{get_col_indexes_df}}).
#' @param x A dataframe.
#' @param columns A vector containing column names. You can use partial matching and regular expressions.
#' @position Either a text or a number. Accepted values include "start" to move the columns in the
#'         begining of the dataframe, "end" to move to the end, or a number to specify the position,
#'         that the columns will move (move after this number). See examples.
#'
#' @examples
#' x <- data.frame(V1 = c("a", "b", "c"),
#'                 V2 = c("A", "B", "C"),
#'                 Z3 = c("c", "d", "e"))
#'
#'  move_columns(x, "V1", 2)
#'  move_columns(x, "1$", 2)
#'  move_columns(x, "^Z", "start")
#'
#' @export

move_columns <- function(x, columns, position){
  indexes <- get_col_indexes_df(x, columns)
  if (length(indexes) == 0){
    stop("No columns matched. Check the second argument and repeat.", call. = FALSE)
  }
  moving_cols <- as.data.frame(x[,indexes, drop = FALSE])
  temp_x <- x[,-indexes, drop = FALSE]
  if (position == "start"){
    print("Moving columns to the start ...")
    output <- cbind(moving_cols, temp_x)
  } else if (position == "end"){
    print("Moving columns to the end ...")
    output <- cbind(temp_x, moving_cols)
  } else if (is.numeric(position)) {
    stop_name <- names(x)[position]
    stop_index <- which(names(temp_x) == stop_name)

    left_x <- temp_x[,1:stop_index, drop = FALSE]
    right_x <- temp_x[,(stop_index + 1):ncol(temp_x), drop = FALSE]

    print(paste("Moving columns after, col", stop_name))
    output <- cbind(left_x, moving_cols, right_x)
  } else {
    stop("'position' argument (the 3rd argument) should be either numeric or 'start' or 'end'. \n
         Check the input the run again.", call. = FALSE)
  }
  print("Done!")
  return(output)

}
