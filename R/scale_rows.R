#' Scale rows
#' @decription Select the columns of interest and scale their rows. Scale function takes the default argument
#'       \code{center, scale} plus some additional, common with other function of the package. The output can
#'       be either appended into the input dataframe or you can drop the input keeping only the scaled data.
#' @param x A dataframe with the values to be normalized.
#' @param columns Vector containing column names to be standardized. By default all columns will be selected
#' \code{"..all.."}. You can add a vector with the column names, or a part of the column name that is unique
#' and use partial matching to identify columns (e.g. "values_"). If complete matching is used, specify
#' \code{complete_match} argument to TRUE.
#' @param label Label tag to be added in each collumn. Default value is "nmax_".
#' @param complete_match Logical with default value \code{FALSE} that allows partial matching of the column
#' names for selection of the column for normalizaiton. If set to \code{TRUE}, columns that match exactly with
#' the input will be used.
#' @param center See \code{link{scale}} function.
#' @param scale See \code{link{scale}} function.
#' @param drop_input Select if you want to append the normalized data into the input dataframe or keep only them.
#' Logical with default value \code{FALSE}, meaning that the processed values will be added to the input.
#' @examples
#' df <- data.frame(text = c("A", "B", "C", "D", "E", "F", "G"),
#'                  V1 = c(1, 2, 3, 4, 3, 2, 1),
#'                  V2 = c(5, 6, 4, 3, NA, 6, 7),
#'                  V3 = c(3, 4, 5, 6, 7, 4, 3))
#'
#' scaled_df <- scale_rows(x = df,
#'                         columns = c("V1", "V2", "V3"),
#'                         complete_match = TRUE,
#'                         drop_input = TRUE)
#'
#' scaled_df <- scale_rows(x = df,
#'                         columns = "V",
#'                         complete_match = FALSE)
#' @export

scale_rows <- function(x,
                       columns = "..all..",
                       complete_match = FALSE,
                       center = TRUE,
                       scale = TRUE,
                       label = "scaled_",
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
      column_indexes <- get_col_indexes_df(x, columns)
    }
    working_data <- x[,column_indexes]
    if (length(names(working_data)) < 2) {
      stop(paste(length(names(working_data)), "columns could be matched.
                 Check again columns vector and try again."), call. = FALSE)
    }
    print(paste(length(names(working_data)), "columns are matched. Procceding with scaling ..."))
    }

  # scale
  scaled_data <- as.data.frame(t(scale(t(working_data), center = center, scale = scale)))

  # prepare export
  print("Correcting names ... ")
  names(scaled_data) <- paste0(label, names(scaled_data))
  print("Merging tables ... ")
  if (drop_input == "FALSE") {
    output <- cbind(x, scaled_data)
  } else {
    output <- scaled_data
  }
  print("Done!")

  return(output)
}
