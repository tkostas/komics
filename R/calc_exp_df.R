calc_exp_df <- function(data,
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
  excl_list <- exclude_cols(x = data, exclusion, complete_match)
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
