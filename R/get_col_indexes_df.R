#' Get column indexes of a dataframe
#'
#' @description Reads a dataframe and returns the column indexes that match to a specific pattern.
#' If input is not a dataframe, returns an error. You can set the first argument as.data.frame(x).
#'
#' @param x A dataframe to with the columns of interest
#' @param id Vector contaning character identifiers for the columns of interest.
#'
#' @examples
#' x <- data.frame(one = 1, two = 2, three = 3, four = 4)
#' vect <- c("o", "wo")
#'
#' get_col_indexes(x, "wo")
#' get_col_indexes(x, vect)
#'
#' @export

get_col_indexes_df <- function(x, id) {
  if (any(class(x) %in% c("data.frame", "tbl", "tbl_df"))) {
    indexes <- unique(grep(paste(id, collapse = "|"), colnames(x)))
    print(paste(length(indexes), "columns matched"))
    return(indexes)
  } else {
    stop("Input is not a dataframe. Set as.data.frame and run again.", call. = FALSE)
  }
}

# example
# x <- data.frame(one = 1, two = 2, three = 3, four = 4)
# vect <- c("o", "wo")
#
# get_col_indexes(x, "wo")
# get_col_indexes(x, vect)
#
# y <- as.list(x)
# get_col_indexes(y, vect)


