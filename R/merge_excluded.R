merge_excluded <- function(excl_list, proc_data, append = FALSE) {
  if (is.null(excl_list$excl_table)) {
    print("All columns are selected for export ...")
    if (append == TRUE) {
      print("Appending proccessed data into the startind data frame ...")
      output <- cbind(excl_list$in_use_table, proc_data)
    } else {
      output <- proc_data
    }

  } else {
    print("Merging tables ...")
    excl_table <- as.data.frame(excl_list$excl_table)
    colnames(excl_table) <- excl_list$excl_names
    if (append == TRUE) {
      print("Appending proccessed data into the startind data frame ...")
      output <- cbind(excl_table, excl_list$in_use_table, proc_data)
    } else {
      output <- cbind(excl_table, proc_data)
    }
  }
  return(output)
}

