#' Test for keyword enrichment in the differentially abundant/expressed ids
#'
#' @description Reads a list of keywords and tests for enrichment in a subset of genes/proteins over
#' the background. As background can be selected the total list of ids or the complete genome/proteome.
#' Significant enrichment is tested using Fisher's exact test. Calculated p-values are furhter adjusted
#' for multiple hypothesis testing error. See arguments for more info.
#'
#' @param kwd_col_test A vector containing keywords for the differentially abundant/expressed ids. Multiple
#'            keywords can be included for each cell, separated by and identifier (by default '; '). Each
#'            element of the vector can contain keywords for a specific protein. You can add filter your
#'            dataset to the ids of interest, add annotation using \code{append_cols_df()} function, and
#'            add assign the corresponding column (e.g. \code{kwd_col_test = my_data$keywords}).
#' @param kwd_col_bg Similar with the \code{kwd_col_test}, is a vector containing keywords for the ids of
#'            background genome/proteome.
#' @param kwd_vector A character vector containing the keywords that you want to test for significance.
#'            Each keyword will be matched exactly, but there is an elasticity for the use of upper
#'            or lower case characters.
#' @param kwd_sep In case that multiple keywords are provided for each protein/gene, you can choose the
#'            keyword seperator. Default value is "; ".
#' @param padj_method Keywords will be tested for significance using Fisher's exact test. P-values will
#'            be further adjusted, by default using "bonverroni" method. See \?p.adjust() for more info.
#'
#' @examples
#' kwd_enrichment(kwd_col_test = significant_ids$keywords,
#'                kwd_col_bg = background_set$keywords,
#'                kwd_vector = c("proteolysis", "transport", "immune system"))
#'
#' @export


kwd_enrichment <- function(kwd_col_test,
                           kwd_col_bg,
                           kwd_vector,
                           kwd_sep = "; ",
                           padj_method = "bonferroni") {
  ## temp functions
  split_keywords <- function(working_list){
    for (i in seq_along(working_list)) {
      working_list[[i]] <- strsplit(working_list[[i]], split = kwd_sep)[[1]]
    }
    return(working_list)
  }

  calculate_frequency <- function(x, keyword){
    true_false <- vector(mode = "numeric", length = length(x))
    for (i in seq_along(x)) {
      true_false[i] <- tolower(keyword) %in% tolower(x[[i]])
    }
    return(sum(true_false))
  }



  ## prepare data
  # for each input dataframe find keyword column and convert it to list
  kwd_test <- as.list(kwd_col_test)
  kwd_bg <- as.list(kwd_col_bg)

  # for each element of the list split keywords into vector
  print("Processing keywords from test group ...")
  kwd_test_list <- split_keywords(kwd_test)
  print("Processing keywords from the background set ...")
  kwd_bg_list <- split_keywords(kwd_bg)



  # make empty vectors for frequencies and calculate frequencies
  print("Preparing temporary files ...")
  n_keywords <- length(kwd_vector)

  # calculate frequencies for test group
  f_test <- vector(mode = "numeric", length = n_keywords)
  for (i in seq_along(f_test)) {
    f_test[i] <- calculate_frequency(kwd_test_list, kwd_vector[i])
  }

  # calculate frequencies for background group
  f_bg <- vector(mode = "numeric", length = n_keywords)
  for (i in seq_along(f_bg)) {
    f_bg[i] <- calculate_frequency(kwd_bg_list, kwd_vector[i])
  }

  output <- data.frame(keyword = kwd_vector,
                       frequency_test_group = f_test,
                       observations_test_group = length(kwd_test_list),
                       frequency_background_group = f_bg,
                       observations_background_group = length(kwd_bg_list))
  output$ratio_test_group <- output$frequency_test_group / output$observations_test_group
  output$ratio_background_group <- output$frequency_background_group / output$observations_background_group
  output$fold_difference <- output$ratio_test_group / output$ratio_background_group

  # testing for significance using Fisher's exact test
  print("Testing for significance using Fisher's exact test ...")
  Ftest_pval <- vector(length = n_keywords)
  for (i in seq_along(kwd_vector)) {
    data <- matrix(c(output[i, "frequency_test_group"],
                     output[i, "observations_test_group"],
                     output[i, "frequency_background_group"],
                     output[i, "observations_background_group"]),
                   nrow = 2,
                   byrow = FALSE)
    Ftest_pval[i] <- fisher.test(data)$p.value
  }
  output <- cbind(output, Ftest_pval)

  output <- output[order(output$Ftest_pval),]
  output[,paste0("Ftest_pval_adj_", padj_method)] <- p.adjust(output$Ftest_pval,
                                                              method = padj_method,
                                                              n = n_keywords)
  return(output)
}
