#' Perform Kruskal-Wallis rank sum test in multiple rows and correct p-values
#'
#' @description Perform Kruskal-Wallis rank sum test in multiple rows and correct p-values.
#'
#' @param x Input dataframe
#' @param groupA Character vector containing identifiers from samples of groupA. You can also use partial matching.
#' @param groupB Character vector containing identifiers from samples of groupB. You can use partial matching.
#' @param complete_match Logical with default value FALSE, allowing partial matching for groupA and groupB.
#' If you want to use exact match, switch to TRUE.
#' @param padj_method Method for adjustment of p-values for multiple hypothesis testing error (see
#' \code{link{p.adjust}} for more info). Default method is "BH".
#'
#'
#' @export

calc_kruskal_test <- function(x, groupA, groupB,
                              complete_match = FALSE,
                              padj_method = "BH") {
  if (typeof(group_list) != "list") {
    stop("group_list should be a list with two elements.", call. = FALSE)
  }
  if (length(group_list) != 2){
    stop("group_list should be a list with two elements.", call. = FALSE)
  }
  if (complete_match == FALSE) {
    print("Searching for samples of group A...")
    samples_A <- get_col_indexes(x, group_list[[1]])
    print("Searching for samples of group B...")
    samples_B <- get_col_indexes(x, group_list[[2]])
    sample_indexes <- c(samples_A, samples_B)
    groups_vector <- c(rep("groupA", times = length(samples_A)), rep("groupB", times = length(samples_B)))
  } else {
    samples_A <- which(names(x) %in% groupA)
    samples_B <- which(names(x) %in% groupB)
    sample_indexes <- c(samples_A, samples_B)
    groups_vector <- c(rep("groupA", times = length(samples_A)), rep("groupB", times = length(samples_B)))
  }
  p_values <- vector(mode = "numeric", length = nrow(x))
  for (i in 1:nrow(x)) {
    sample_values <- as.vector(as.matrix(x[i, sample_indexes]))
    p_values[i] <- kruskal.test(x = sample_values,
                                g = as.factor(groups_vector))$p.value
  }
  proc_data <- cbind(x, p_values)
  proc_data <- proc_data[order(proc_data$p_values),]
  proc_data$p_adj <- p.adjust(proc_data$p_values, method = padj_method)
  print("Done.")
  return(proc_data)
}
