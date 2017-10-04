#' Label each row based on specific rules
#' @description Reads a dataframe 'x' and based on the rules described in the 'thresholds' list
#'   will create a new column and add the corresponding label. Keep the same order in the thresholds
#'   and labels list.
#'
#' @param x A data.frame containing the values to be tested.
#' @param thresholds A list of lists (see examples). The 1st level of elements in this list describes
#'               the category of labeling. The 2nd level, contains the rules that need to be TRUE.
#'               The names of the 2nd level lists should match with the columns that will be used for
#'               the comparison (see example). The values in the 2nd level list can be either text
#'               or can contain a symbol for comparison and a number (e.g. "> 0.5"). Symbols recognized,
#'               include '>', '<' and '='.
#' @param labels A list with the corresponding labels. If the rules from the thresholds list are TRUE,
#'               the corresponding value of the labels list will be added in the data.frame. Keep the
#'               same order with the thresholds. If the 1st list of thresholds is positive, the 1st label
#'               will be added. The last entry in the labels list should be labeled 'colname' and contains
#'               the column name of the new column that will be added. See example for more info.
#'
#' @examples
#' x <- data.frame(pval = seq(from = 0.1, to = 1, by = 0.1),
#'                 group = c(rep("A", times = 5), rep("B", times = 5)))
#'
#' thresholds = list(over = list(pval = ">0.7",
#'                               group = "B"),
#'                   under = list(pval = "<0.4",
#'                                group = "A"))
#'
#' labels = list(over = "increased",
#'               under = "decreased",
#'               colname = "Difference")
#'
#' over_under(x)
#' @export



over_under <- function(x, thresholds = list(over = list(pval = ">0.7",
                                                        group = "B"),
                                            under = list(pval = "<0.4",
                                                         group = "A")),
                       labels = list(over = "increased",
                                     under = "decreased",
                                     colname = "Difference")) {
  # check that the thresholds list is correct
  print("Checking if arguments class...")
  if (class(thresholds) != "list"){
    stop("'threshold' argument should be a list. See help for more info.", call. = FALSE)
  }

  ### define some helping functions
  # read the test value from the dataframe and the value from the rule
  ### function will return true or false
  make_comparison <- function(test_value, value){
    print("Preparing comparison ...")
    ### is the first letter symbol?
    first_element <- substr(value, 1, 1)
    if (first_element %in% c(">", "<", "=")) {
      first_element_type <- "symbol"
    } else {
      first_element_type <- "text"
    }
    #### extract the test value
    if (first_element_type == "symbol") {
      number <- as.numeric(substr(value, 2, nchar(value)))
    } else {
      txt <- as.character(value)
    }
    #### extract the rule
    if (first_element_type == "symbol") {
      if (first_element == ">") {
        test <- test_value > number
      } else if (first_element == "<") {
        test <- test_value < number
      } else {
        test <- test_value == number
      }
    } else {
      test <- test_value == txt
    }
    return(test)
  }


  # every every row in the dataframe
  r <- 1
  new_column <- vector(mode = "character", length = nrow(x))
  for (r in 1:nrow(x)){
    print(paste("Checking row number:", r))
    for (i in 1:length(thresholds)){ # starting with the first category
      print(paste("> Checking rule", i))
      comparison <- TRUE
      for (j in seq_along(thresholds[[i]])) { # check for every rule
        column_name <- names(thresholds[[i]][j])
        test_value <- x[r, column_name]
        if (is.na(test_value)) {
          break
        } else {
          value <- thresholds[[i]][[j]]
          comparison <- make_comparison(test_value, value)
          #print(paste("test_value", test_value))
          #print(paste("value", value))
        }

        # if the comparison fails, give j the max value and terminate the loop
        if (comparison == FALSE) {
          break
        }
        if (j == length(thresholds[[i]]) & comparison == TRUE) {
          new_column[r] <- labels[[i]]
        }
      }
      i <- i + 1
    }
    r <- r + 1
  }
  print("Preparing output ...")
  output <- cbind(x, new_column)
  colnames(output)[ncol(output)] <- labels$colname
  print("Done!")
  return(output)
}
