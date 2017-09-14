# data <- x
# head(x)
#
# bbb <- exclude_cols(x, exclusion = "Intensity.SC_Ctrl_06")
# str(bbb)

# str(aaa$in_use_table)
# str(bbb$excl_table)
# exclusion is a vector
exclude_cols <- function(x, exclusion = c(), complete_match = TRUE) {
  # check exclusion list
  # output a list iwn excl_table and in_use_table
  output <- list()
  excl_indx <- c()
  if (is.null(exclusion)) {
    print("No columns are excluded ...")
    output[["excl_table"]] <- c()
    output[["in_use_table"]] <- x
  } else {
    if (complete_match == TRUE) {
      i <- 1
      for (i in 1:(length(exclusion))) {
        #find indexes
        indx <- grep(exclusion[i], colnames(x))
        excl_indx <- c(excl_indx, indx)
      }
    } else {
      excl_indx <- get_col_indexes(x, exclusion)
    }
    un_indx <- unique(excl_indx)
    print(paste(length(un_indx), "columns found in exclusion list ..."))
    output[["excl_table"]] <- x[,un_indx]
    output[["excl_names"]] <- colnames(x[un_indx])
    output[["in_use_table"]] <- x[,-un_indx]
  }
  return(output)
}
