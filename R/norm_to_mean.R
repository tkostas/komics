# source("func_list.r")
#
#
# data <- read.csv(file.choose(), header = TRUE)
# colnames(data)
# require(purrr)
#
# #calc_log2_df
# x <- data[,4:ncol(data)]
# colnames(x)
# z <- calc_log2_df(x, log_lab = "log_")
# x <- z
# custom_ref <- 1
# custom_ref <- "l_03"
# norm_lab <- "norm_"
# colnames(z)
#
# head(z[,1])
# head(y[,1])
# colnames(z)
# colnames(y)
#
# y <- norm_to_mean(z, custom_ref = custom_ref)
#
norm_to_mean <- function(x,
                         custom_ref = 1,
                         norm_lab = "norm_",
                         rm_zero = TRUE,
                         exclusion = c(),
                         complete_match = TRUE,
                         append = FALSE){
  print("---------------------")
  print("Normalizing dataset to mean intensity ...")
  print("---------------------")
  # make exclusion list
  # prepare exclusion list
  excl_list <- exclude_cols(x, exclusion, complete_match)
  in_use_table <- excl_list$in_use_table

  # check if input is numeric
  check_if_numeric_df(in_use_table)

  # caclculate mean for each column
  print("Calculating column means ...")
  mean_int_vec <- apply(in_use_table, 2, mean, na.rm = TRUE)

  # find reference
  print("Finding reference column ...")
  ref_col <- custom_ref
  if (ref_col == 1) {
    print("First sample is set as reference.")
    indx <- custom_ref
    ref_col <- mean_int_vec[indx]
  } else if (length(grep(custom_ref, names(mean_int_vec))) == 1) {
    indx <- grep(custom_ref, names(mean_int_vec))
    ref_col <- mean_int_vec[indx]
    print(paste(names(mean_int_vec)[indx], "selected as reference sample."))
  } else {
    stop("Check the reference vector identifier and run again.")
  }

  # calculate correction factor
  print("Calculating correction factor ...")
  cor_factor <- mean_int_vec - ref_col

  # normalize values
  print("Normalizing values ...")
  i <- 1
  norm_x <- matrix(nrow = nrow(in_use_table))
  norm_vec <- vector(length = nrow(in_use_table))
  for (i in seq_along(colnames(in_use_table))) {
    TF <- in_use_table[,i] == 0
    norm_vec <- in_use_table[,i] - cor_factor[i]
    norm_vec[TF] <- 0
    norm_x <- cbind(norm_x, norm_vec)
  }
  normalized_df <- as.data.frame(norm_x[,2:ncol(norm_x)])
  # rm(list = c(norm_x, norm_vec))
  head(normalized_df)
  # correcting column names
  print("Correcting column names ...")

  new_names <- vector(length = length(colnames(in_use_table)))
  i <- 1
  for (i in seq_along(colnames(in_use_table))) {
    new_names[i] <- paste0(norm_lab, colnames(in_use_table)[i])
  }
  colnames(normalized_df) <- new_names

  # remove 0 if selected
  if (rm_zero == TRUE) {
    print("Removing zero values ...")
    proc_data <- convert_to_df(normalized_df, value = 0, to = NA)
  } else {
    proc_data <- convert_to_df(normalized_df, value = NA, to = 0)
  }

  output <- merge_excluded(excl_list, proc_data, append = append)
  print("Normalization to mean, Done!")
  return(output)


}


