#' Convert selected values from a dataframe
#' @description Convert selected values to target value. Input and output are both dataframes.
#' By default 0 is replaced with \code{NA}. Non-numeric column names should be included in the exclusion
#' argument. Columns can be selected either by exact name matching or by partial matching. For
#' partial matching turn \code{complete_match = FALSE}.
#'
#' @param x A dataframe with values.
#' @param lab Label to be added in each column after conversion. By default nothing is added.
#' @param value Value to be changed. Default is 0.
#' @param to Target value. Default is \code{NA}.
#' @param exclusion Vector containing labels from the columns that should be excluded. Even a partial match is enough.
#' @param complete_match Logical with default value \code{TRUE}. If exclusion vector contains exactly
#' the column names of the excluded columns, leave the \code{complete_match} to \code{TRUE}.
#' For partial matching of the columns switch to \code{FALSE}.
#'
#' @examples
#' new_data <- convert_to_df(x, value = 0, to = NA, exclusion = c(“Protein”, “pept”))



convert_to_df <- function(x, lab = "", value = 0, to = NA, exclusion = c(), complete_match = TRUE) {
  print("---------------------")
  print(paste("Converting", value, "to", to))
  print("---------------------")
  # make exclusion list
  excl_list <- exclude_cols(x, exclusion, complete_match)

  # check if values are numeric
  print("Checking if values are numeric ...")
  i <- 1
  in_use_table <- excl_list$in_use_table
  in_use_table <- as.data.frame(in_use_table)
  for (i in seq_along(colnames(in_use_table))) {
    if (!is.numeric(in_use_table[,i])) {
      stop(paste0("Column '", colnames(in_use_table[i]), "' should be numeric. Check if there is any text."))
    }
  }
  print("Ok columns are numeric!")

  # calculate log2
  print("Converting values ...")
  proc_data <- apply(excl_list$in_use_table, 2, convert_to_vec, value = value, to = to)

  # correlct labels
  print("Correcting names ...")
  proc_labs <- vector(length = length(colnames(proc_data)))
  i <- 1
  for (i in seq_along(colnames(proc_data))) {
    proc_labs[i] <- paste0(lab, colnames(proc_data)[i])
  }
  colnames(proc_data) <- proc_labs
  output <- merge_excluded(excl_list = excl_list, proc_data = proc_data)

    print("Done!")
  return(as.data.frame(output))
}


# example
# convert_to_df(x, lab = "test_", value = 537320, to = -1111111)

