#' Calculate row frequencies
#'
#' @description Identified samples for each group, reads rows and calculates the number of values above a
#' certain threshold.
#'
#' @param x A dataframe with the values to be normalized.
#' @param sample_vector Vector containing column names to be used. Column names should follow the same order
#' with the groups defined in the \code{groups_vector} argument. For example, sample names and corresponding
#' groups can be saved on one table and refer to the specific columns. If complete matching is used, specify
#' \code{complete_match} argument to TRUE.
#' @param group_vector Vector containing goup names. Group_vector should have the same length with the
#' \code{sample_vector}.
#' @param label Character tag to be added in the columns containing the mean group frequencies. Default value is "f_".
#' @param complete_match Logical with default value \code{FALSE} that allows partial matching of the column
#' names for selection of the column for normalizaiton. If set to \code{TRUE}, columns that match exactly with
#' the input will be used.
#' @param threshold Threshold with default value = 0. By default the number of observations with frequency higher
#' than 0 will be calculated.
#' @param drop_input Select if you want to append the normalized data into the input dataframe or keep only them.
#' Logical with default value \code{FALSE}, meaning that the processed values will be added to the input.
#'
#' @examples
#' data <- data.frame(L1 = c("a", "b", "c", "d", "e", "f", "g", "h"),
#'                     V1 = c(1, 2, 3, 6, 1, 3, 4, 4),
#'                     V2 = c(3, 4, 5, 3, 5, 5, 3, 2),
#'                     V3 = c(6, 7, 4, 2, 3, 2, 7, 6))
#' calc_row_frequency(x = data, sample_vector = "V", group_vector = "my_group")
#' calc_row_frequency(x = data, sample_vector = "V", group_vector = "my_group", threshold = 2)
#'
#'
#' @export


calc_row_frequency <- function(x,
                               sample_vector,
                               group_vector,
                               label = "f_",
                               complete_match = FALSE,
                               threshold = 0,
                               drop_input = FALSE){
  # find groups sample indexes and prepare working vector
  unique_groups <- unique(group_vector)
  print(paste(length(unique_groups), "groups found..."))
  frequency_table <- matrix(nrow = nrow(x), ncol = length(unique_groups))
  colnames(frequency_table) <- paste0(label, unique_groups)

  for (i in 1:length(unique_groups)) {
    if (complete_match == TRUE) {
      column_indexes <- which(names(x) %in% sample_vector[group_vector == unique_groups[i]])
    } else if (complete_match == FALSE) {
      column_indexes <- get_col_indexes_df(x, sample_vector[group_vector == unique_groups[i]])
    }
    working_data <- x[,column_indexes]
    frequency_table[,i] <- apply(working_data, 1, function(x, threshold = 0) sum(x > threshold, na.rm = TRUE))
  }

  # prepare export
  print("Merging tables ... ")
  if (drop_input == "FALSE") {
    output <- cbind(x, frequency_table)
  } else {
    output <- frequency_table
  }
  print("Done!")

  return(output)
}
