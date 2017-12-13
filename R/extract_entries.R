#' Extract the entries of a dataframe that match specific rules
#' @description Read a dataframe and extract from the specified column and entrie that match
#'         a set of rules specified in the \code{rules_list} argument. See example for more details.
#'         Keep the column names of the 2nd level list of the rules_list as they are in the example.
#' @param x A dataframe containing all the values to be examined.
#' @param lookup_col Column name of the column with the identifiers. Match the name exactly.
#' @param rules_list A list of lists containing the rules for the selection of identifiers. See
#'          example for more details. Every element of the rules_lest is a list with four elements.
#'          Keep the names of each of these element as it is in the example. In info describe the
#'          comparison. This is just for you to remember what you are comparing, so just write a text.
#'          The column element is used to match the column of the dataframe x that will be used for the
#'          evaluation. The rule can take values ">=", ">", "=", "<", "<=", "in", "not in".
#'          The value element of the list is the value that will be compared. It can be a single
#'          element vector or a vector with multiple elements. Depending on the comparison it can
#'          be numeric or character vector. See example.
#' @param return_summary Default value is FALSE. If set to TRUE the output will be a dataframe with
#'          the number of elements that match the comparison.
#'
#'
#' @examples
#' rules <- list(comp1 = list(info = "Is greter than 3",
#'                            column = "y",
#'                            rule = ">=",
#'                            value = "3"),
#'               comp2 = list(info = "Is equal to e",
#'                            column = "z",
#'                            rule = "=",
#'                            value = "e"),
#'               comp3 = list(info = "Is either b, d or f",
#'                            column = "z",
#'                            rule = "in",
#'                            value = c("b", "d", "f")),
#'               comp4 = list(info = "Is not c or d",
#'                            column = "z",
#'                            rule = "not in",
#'                            value = c("c", "d")))
#'
#' x <- LETTERS[seq(1:6)]
#' y <- seq(1:6)
#' z <- letters[seq(1:6)]
#' d <- data.frame(x = x, y = y, z = z)
#'
#' matched_ids_list <- extract_entries(x = d,
#'                                     lookup_col = "x",
#'                                     rules_list = rules)
#'
#' @export

extract_entries <- function(x,
                            lookup_col,
                            rules_list,
                            return_summary = FALSE) {
  print("Read the example in the description. I assume that the input is correct.")
  # check x is correct
  #   to be added
  # check that lookup_col is matched
  #   to be added
  # check that rules_list have the proper format
  #   to be added

  output_list <- list()
  for (i in seq_along(rules_list)) {
    temp_list <- list(info = c(),
                      ids = c())
    temp_list[["info"]] <- rules_list[[i]][["info"]]

    if (rules_list[[i]][["rule"]] == ">") {
      test_col <- rules_list[[i]][["column"]]
      indexes <- x[,test_col] > rules_list[[i]][["value"]]
    } else if(rules_list[[i]][["rule"]] == ">=") {
      test_col <- rules_list[[i]][["column"]]
      indexes <- x[,test_col] >= rules_list[[i]][["value"]]
    } else if(rules_list[[i]][["rule"]] == "=") {
      test_col <- rules_list[[i]][["column"]]
      indexes <- x[,test_col] == rules_list[[i]][["value"]]
    } else if(rules_list[[i]][["rule"]] == "<=") {
      test_col <- rules_list[[i]][["column"]]
      indexes <- x[,test_col] <= rules_list[[i]][["value"]]
    } else if(rules_list[[i]][["rule"]] == "<") {
      test_col <- rules_list[[i]][["column"]]
      indexes <- x[,test_col] < rules_list[[i]][["value"]]
    } else if(rules_list[[i]][["rule"]] == "in") {
      test_col <- rules_list[[i]][["column"]]
      indexes <- x[,test_col] %in% rules_list[[i]][["value"]]
    } else if(rules_list[[i]][["rule"]] == "not in") {
      test_col <- rules_list[[i]][["column"]]
      indexes <- !(x[,test_col] %in% rules_list[[i]][["value"]])
    } else {
      stop(paste("Error in comparison number: ", i, ". The comparison rule is not in the correct input.", .call = FALSE))
    }
    ids <-x[indexes, lookup_col]
    temp_list[["ids"]] <- as.character(ids)
    output_list[[i]] <- temp_list
  }

  if (return_summary == TRUE) {
    summary_df[i, 1] <- output_list[[i]][[1]]
    list_length <- length(output_list)
    summary_df <- data.frame(info = vector(length = list_length),
                             number_of_ids = vector(length = list_length))
    for (i in seq_along(output_list)) {
      summary_df[i, 1] <- output_list[[i]][[1]]
      summary_df[i, 2] <- length(output_list[[i]][[2]])
    }
    print("Done. Returning summary as a dataframe.")
    return(summary_df)
  }
  print("Done. Output is a list.")
  return(output_list)
}
