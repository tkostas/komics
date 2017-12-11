#' Keyword enrichment for multiple categories
#' @description Reads multiple category identifiers and performes a keyword enrichment
#'    for each combination of test/background group (see \code{\link[komics]{kwd_enrichment}}).
#'    The output is a list of tables.
#'
#' @param x A dataframe with the values.
#' @param categories A character vector containing the categories that will be tested.
#' @param test_keywords A caracter vector with the keywords to be tested.
#' @param grouping_col A character string matching exactly the column of the dataframe \code{x}
#'                 that contains the labaled for each category.
#' @param keywords_col A character string matching exactly the column of the dataframe \code{x}
#'                 that contains the keywords.
#' @param padj_method Method to adjust the p-values of the enrichment test (Fisher exact test).
#'                 See \code{link{p.adjust}} for more info.
#'
#' @examples
#' \donttest{
#'   test_keywords <- c("protease",
#'                      "solute binding protein",
#'                      "oxidoreductase",
#'                      "transport",
#'                      "virulence")
#'
#'   categories <- c("stable", "decreasing", "increasing")
#'   category_comparison <- kwd_enrichment_multi(x = merged_data,
#'                                               categories = categories,
#'                                               grouping_col = "Grouping_col",
#'                                               test_keywords = test_keywords)
#'
#' }
#' @export
kwd_enrichment_multi <- function(x,
                                 categories,
                                 test_keywords,
                                 grouping_col,
                                 keywords_col = "keywords",
                                 padj_method = "BH") {
  # loop for each element of the categories
  # and performed a keyword enrichment test
  output_list <- list()
  for (i in seq_along(categories)) {
    #select a subcategory for the test
    subgroup <- categories[i]
    print("--------------------------------------------")
    print(paste0("Testing group: ", i, " '", subgroup, "'"))

    # split to test and background groups
    if(!exists("grouping_col")) {
      stop("grouping_col argument is not defined.", .call = FALSE)
    }
    if(!grouping_col %in% names(x)) {
      stop("grouping_col argument does not match with any of the column names", .call = FALSE)
    }
    indexes <- x[,grouping_col] == subgroup
    print(paste(nrow(x), "entries in total."))
    filtered_data <- x[indexes,]
    print(paste(nrow(filtered_data), "entries in test set."))
    bg_data <- x[!indexes, ]
    print(paste(nrow(bg_data), "entries in background set."))

    # extract test vector and bg_vector for the keyword enrichment
    test_vector <- as.vector(filtered_data[,keywords_col])
    bg_vector <- as.vector(bg_data[,keywords_col])

    # run keyword enrichment
    kwd_table <- kwd_enrichment(kwd_col_test = test_vector,
                                kwd_col_bg = bg_vector,
                                kwd_vector = test_keywords,
                                padj_method = padj_method)
    output_list[[i]] <- kwd_table
  }
  print("--------------------------------------------")
  print("Done. The individual tables are saved in the output as a list.")
  return(output_list)
}
