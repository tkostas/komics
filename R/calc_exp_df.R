#' Calculate exponential values from a dataframe
#'
#' @description Reads a data frame and calculates the exponential value (basex) for
#' the defined columns. Non-numeric columns can be excluded by adding the column name
#' in exclusion vector. For partial matching of column names use
#' \code{complete_match = FALSE}. See \code{calc_exp_vec}.
#'
#' @param x 	Input data frame. It can contain also non-numeric columns.
#' @param base Base number for which you will calculate the exponential value.
#' Default value is 2.
#' @param sub Character string to substitute in column names. Default is empty
#' @param replacement Character string to replace the substituted one. Default is empty.
#' @param exclusion Vector containing labels from the columns that should be excluded.
#'  Even a part of the label is enough.
#' @param complete_match Logical with default value \code{TRUE}. If exclusion vector
#' contains exactly the column names of the excluded columns, leave the
#' \code{complete_match} to \code{TRUE}. For partial matching of the columns switch
#' to \code{FALSE}.
#' @param append Logical with default value \code{FALSE}. If set \code{TRUE} processed
#' data will be added in the starting dataframe. If \code{TRUE}, the processed columned
#' will be merged only with the excluded ones.
#'
#'
#' @examples
#' y1 <- calc_exp_df(x = my_data,
#'                   base = 2,
#'                   sub = "norm_log_",
#'                   replacement = "norm_",
#'                   exclusion = colnames(y)[1:3],
#'                   complete_match = TRUE)
#'
#' @export

calc_exp_df <- function(x,
                        base = 2,
                        sub = "",
                        replacement = "",
                        exclusion = c(),
                        complete_match = TRUE,
                        append = FALSE){
  print("---------------------")
  print("Calculated exponential values ...")
  print("---------------------")
  # prepare exclusion list
  excl_list <- exclude_cols(x = x, exclusion, complete_match)
  in_use_table <- excl_list$in_use_table

  # print checking if columns are numeric
  check_if_numeric_df(in_use_table)

  # calculate exponential
  print("Calculating exponential values ...")
  output <- matrix(ncol = ncol(in_use_table), nrow = nrow(in_use_table))
  i <- 1
  for (i in seq_along(colnames(in_use_table))) {
    output[,i] <- calc_exp_vec(in_use_table[,i], base = base)
  }
  # correcting names
  print("Correcting names ...")
  column_names <- colnames(in_use_table)
  print(paste0("Replacing '", sub, "' to '", replacement, "'"))
  hits <- length(grepl(sub, column_names))
  print(paste0(hits, " columns match with '", sub, "'"))
  new_names <- gsub(sub, replacement, column_names)
  colnames(output) <- new_names
  output2 <- merge_excluded(excl_list, output, append = append)
  n <- nrow(output2)
  p <- ncol(output2)
  print(paste("Done! Ouput contains", n, "rows and", p, "columns."))
  return(output2)
}

