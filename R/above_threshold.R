#' Calculate number of observations above a certain threshold
#'
#' @description Reads a data frame and calculates the number of observations that are above a certain threshold value.
#' Frequency is calculated for columns or rows. Groups can be assigned in the group_ids argument, by passing
#' a list with the corresponding info (see arguments).
#'
#' @return Output is a list and each element is a vector containing
#' the sum of observations that are above the threshold. Each element has the same name as it was defined in the
#' group_ids list, and omitting naming the elements of the list will return an error. This way you can calculate
#' frequencies for different groups in parallel.
#'
#' @param x A dataframe. You can select which columns to be used using the group_ids argument.
#' @param apply_to Character vector. Calculate frequency for "columns", or "rows". Default value is "columns".
#' @param group_ids List of group names and group identifiers (see example). If all the columns are used as one group,
#'  skip this argument. To specific columns for one or more groups define a list in which the name of each list element
#'  will be the group name and every element will contain a character vector that matches with the column names of the specific group.
#'  Matching of the columns can be partial (e.g. ctrl_ to select multiple columns containing the "ctrl_" character string)
#' @param threshold The minimum value to calculate frequency. Default is 0.
#'
#' @examples
#' treatment_group <- c("Tretment", "treated")
#' control_group <- c("ctrl", "control")
#' group_ids <- list(treatment = treatement_group,
#'                   control = control_group)
#'
#' above_threshold(data, group_ids = group_ids, threshold = 0)
#' @export




above_threshold <- function(x, apply_to = "columns", group_ids = NULL, threshold = 0){
  # identify subgroups and get indexes
  index_list <- list()
  if (is.null(group_ids)) {
    print("All columns were selected for the calculation ...")
    index_list[["x"]] <- seq(1:ncol(x))
  } else if (is.list(group_ids)) {
    print(paste(length(group_ids), "group(s) identified."))
    for (i in seq_along(group_ids)) {
      g_name <- names(group_ids)[i]
      g_name
      g_indexes <- get_col_indexes(x = x, id = group_ids[[i]])
      index_list[[paste(g_name)]] <- g_indexes
    }
  } else {
    stop("'groups_ids' argument should be either empty or a list with names for each element.
         Correct and repeat.", call. = FALSE)
  }

  # check if data are numeric
  for (i in seq_along(index_list)) {
    print(paste("Checking group", i))
    check_if_numeric_df(x[,index_list[[i]]])
  }
  # calculate above_threshold_vec
  frequency_list <- list()
  if (apply_to == "columns") {
    print("Calculating frequency for columns with value greater than ", threshold)
    for (i in seq_along(index_list)) {
      new_entry_name <- names(index_list[i])
      frequency_list[[new_entry_name]] <- apply(x[, index_list[[i]]], 2, above_threshold_vec, y = threshold)
    }
  } else if (apply_to == "rows") {
    print("Calculating frequency for rows ...")
    for (i in seq_along(index_list)) {
      new_entry_name <- names(index_list[i])
      frequency_list[[new_entry_name]] <- apply(x[, index_list[[i]]], 1, above_threshold_vec)
    }
  } else {
    stop("apply_to attribute should be either 'columns' or 'rows'.", call. = FALSE)
  }
  # merge and output
  print("Calculation is done. Output is a list.")
  return(frequency_list)
}
