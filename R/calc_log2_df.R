#' Calculate log2 values in a dataframe
#' @description Reads a data-frame and calculates log2 for each value. Non-numeric columns that
#' should be transformed can be excluded by adding the column name in exclusion vector. For partial
#' matching of column names use \code{complete_match = FALSE}. Transformed columns can be renamed
#' using the \code{log_lab} argument. Missing values can be either replaced with 0 or removed by
#' adjusting the \code{rm_zero} argument.
#'
#' @param x Dataframe containing only numeric values.
#' @param log_lab Character tag to be added in front of every column name of the dataframe (e.g. "Log2_".
#' @param rm_zero If \code{TRUE}, missing values will remain \code{NA}. If set to \code{FALSE}
#' missing values will be replaced by 0. By default \code{rm_zero = TRUE}.
#' @param exclusion Vector containing labels from the columns that should be excluded. Even a part of the label is enough.
#' @param complete_match Logical with default value \code{TRUE}. If exclusion vector contains exactly
#' the column names of the excluded columns, leave the \code{complete_match} to \code{TRUE}. For partial
#' matching of the columns switch to \code{FALSE}
#' @param append Logical with default value \code{FALSE}. If set \code{TRUE} processed data will be
#' added in the starting dataframe. If \code{FALSE}, the processed columned will be merged only with
#' the excluded ones.
#'
#' @examples
#' calc_log2_df(x, log_lab = "my_label", rm_zero = FALSE)
#' @export

calc_log2_df <- function(x, log_lab,
                         rm_zero = TRUE,
                         exclusion = c(),
                         complete_match = TRUE,
                         append = FALSE) {
    # check if values are numeric
    print("---------------------")
    print("Converting values to Log2 ...")
    print("---------------------")
    # prepare exclusion list
    excl_list <- exclude_cols(x, exclusion, complete_match)
    in_use_table <- excl_list$in_use_table

    # start calculation
    print("Checking if values are numeric ...")
    i <- 1
    for (i in seq_along(colnames(in_use_table))) {
      if (!is.numeric(in_use_table[[i]])) {
        stop(paste0("Column '", colnames(in_use_table)[i], "' should be numeric. Check if there is any text."))
      }
    }

    # calculate log2
    print("Converting values to log2 ...")
    log2_data <- apply(in_use_table, 2, calc_log2_vec)

    # correlct labels
    print("Correcting names ...")
    log_labs <- c()
    i <- 1
    for (i in seq_along(colnames(log2_data))) {
      log_labs[i] <- paste0(log_lab, colnames(log2_data)[i])
    }
    colnames(log2_data) <- log_labs

    # remove 0 if selected
    output <- matrix(nrow = nrow(log2_data), ncol = ncol(log2_data))
    if (rm_zero == TRUE) {
      print("Removing zero values ...")
      # convert_to_df(x = log2_data, 0, NA)
      i <- 1
      for (i in seq_along(colnames(log2_data))) {
        output_vec <- convert_to_vec(log2_data[,i], 0, NA)
        output[,i] <- output_vec
      }
      colnames(output) <- colnames(log2_data)
    } else {
      output <- log2_data
    }
    output2 <- merge_excluded(excl_list, output, append = append)
    print("Done!")
    return(as.data.frame(output2))
}
