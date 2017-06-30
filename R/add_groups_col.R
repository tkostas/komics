#' Add group names based on experiment name
#'
#' @description Reads the experiments names from a column, and creates a new column with the group
#' name for each experiment. Parital matchins is supported, so groups can be matched even if you add
#' additional tags to the experiment names (e.g. 'log2_').
#'
#' @param x A dataframe that contains one column with experiments names.
#' @param samples_vector A vector containing the experiment names.
#' @param groups_vector A vector containing the group names for each sample of the samples_vector.
#' Info for groups_vector and samples_vector can be stored in a separate dataframe and link the
#' corresponding columns.
#' @param samples_column The column name from the input dataframe that containins the sample names.
#'      Exact match of this column will be used, so write the full name.
#' @param column_name The name of the new column that will include the group name. Default value is
#'      'groups', and this column should not already exist in the dataframe used as input.
#'
#' @examples
#'  grouped_data <- add_groups(x = NS_secreted_long,
#'                             samples_vector = groups_df$headers_raw,
#'                             groups_vector = groups_df$group,
#'                             samples_column = "variable")
#'
#' @export


add_groups_col <- function(x,
                       samples_vector,
                       groups_vector,
                       samples_column,
                       column_name = "groups") {
  # check and add new column
  if (column_name %in% names(x)) {
    stop("Column name already exists, define a new name in the 'column_name' argument and repeat.", call. = FALSE)
  }
  x[,column_name] <- NA
  print(paste0("Column '", column_name, "' is added in the initial table"))

  # add group names in the new column
  for (i in seq_along(samples_vector)) {
    indexes <- grep(samples_vector[i], (as.vector(x[,samples_column])))
    print(paste(length(indexes), "rows matched to", samples_vector[i]))
    x[indexes, column_name] <- groups_vector[i]
  }
  print("Done!")
  return(x)
}
