#' Normalize rows to the maximum value
#'
#' @description Reads rows and normalized the values based on the maximum of the row (range 0-1).
#'
#' @param x A dataframe with the values to be normalized.
#' @param columns Vector containing column names to be standardized. By default all columns will be selected
#' \code{"..all.."}. You can add a vector with the column names, or a part of the column name that is unique
#' and use partial matching to identify columns (e.g. "values_"). If complete matching is used, specify
#' \code{complete_match} argument to TRUE.
#' @param label Label tag to be added in each collumn. Default value is "nmax_".
#' @param complete_match Logical with default value \code{FALSE} that allows partial matching of the column
#' names for selection of the column for normalizaiton. If set to \code{TRUE}, columns that match exactly with
#' the input will be used.
#' @param sub_min Logical with default value \code{TRUE}. By default the calculation is
#' \code{(x - x_min) / (x_max - x_min)}. If \code{sub_min} is set to \code{FALSE} the normalization will be
#' performed as: \code{x / x_max}
#' @param na.rm Action for missing values when calculating min and max values. Defalut is \code{TRUE}
#' @param drop_input Select if you want to append the normalized data into the input dataframe or keep only them.
#' Logical with default value \code{FALSE}, meaning that the processed values will be added to the input.
#'
#' @examples
#' data <- data.frame(L1 = c("a", "b", "c", "d", "e", "f", "g", "h"),
#'                    V1 = c(1, 2, 3, 6, 1, 3, 4, 4),
#'                    V2 = c(3, 4, 5, 3, 5, 5, 3, 2),
#'                    V3 = c(6, 7, 4, 2, 3, 2, 7, 6))
#'  norm_to_max(data, columns = "V")
#'  norm_to_max(data, columns = "V", sub_min = FALSE)
#'
#' @export

norm_to_max <- function(x,
                        columns = "..all..",
                        label = "nmax_",
                        complete_match = FALSE,
                        sub_min = TRUE,
                        na.rm = TRUE,
                        drop_input = FALSE){
  # find indexes and prepare working vector
  if (columns == "..all..") {
    print("All columns selected for normalization...")
    working_data <- x
    }
  else {
    if (complete_match == TRUE) {
      column_indexes <- which(names(x) %in% columns)
    } else if (complete_match == FALSE) {
      column_indexes <- get_col_indexes(x, columns)
    }
    working_data <- x[,column_indexes]
    if (length(names(working_data)) < 2) {
      stop(paste(length(names(working_data)), "columns could be matched.
                 Check again columns vector and try again."), call. = FALSE)
    }
    print(paste(length(names(working_data)), "columns are matched. Procceding with the normalization ..."))
    }

  # normalize
  # define helping functions
  norm_func_1 <- function(z) {
    (z - min(z, na.rm = na.rm)) / (max(z, na.rm = na.rm) - min(z, na.rm = na.rm))
  }
  norm_func_2 <- function(z) {
    z / max(z, na.rm = na.rm)
  }
  # loop
  normalized_data <- working_data
  for (i in 1:nrow(normalized_data)) {
    row_numbers <- normalized_data[i,]
    if (sub_min == TRUE) {
      norm_numbers <- norm_func_1(row_numbers)
    } else if (sub_min == FALSE) {
      norm_numbers <- norm_func_2(row_numbers)
    } else {
      stop("'sub_min' argument should be either TRUE or FALSE. Check '?norm_to_max' for more info.", call. = FALSE)
    }
    normalized_data[i,] <- norm_numbers
  }
  # prepare export
  print("Correcting names ... ")
  names(normalized_data) <- paste0(label, names(normalized_data))
  print("Merging tables ... ")
  if (drop_input == "FALSE") {
    output <- cbind(x, normalized_data)
  } else {
    output <- normalized_data
  }
  print("Done!")

  return(output)
}
