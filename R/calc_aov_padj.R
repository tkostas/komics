#' Perform ANOVA, calculate p-values and adjust for multiple hypothesis testing error
#'
#' @description Reads a dataframe, identifies the sample groups based on the input info,
#' and performs ANOVA to identify rows that show difference in at least one of the groups.
#' P-values are adjusted for multiple hypothesis testing error, and the output is returned
#' as a dataframe. If the number of observations is not enought for the ANOVA test, the entry
#' is labeled as "excluded".
#'
#' @param x A dataframe containing the protein/gene identifier. Each row is one protein/gene and
#' each column is one samlple. Additional columns can be included in this dataframe. The samples
#' will be matched based on the \code{sample_names} vector. However because samples are identified
#' by partial matching (\code(grep) function) try to reduce the number of columns if you encounter an error.
#' @param sample_names A vector containing the sample names.
#' @param group_names A vector of equal length with the \code{sample_names} containing the group name
#' for each sample. You can store in a separate dataframe the sample_names and the group_names and
#' just reference to this table.
#' @param pAdj Method for adjustment of p-values. See more at \code{p.adjust}. Default method is "BH".
#'
#' @examples
#' proc_data <- calc_aov_padj(my_data,
#'                            sample_names = sample_groups$samples,
#'                            group_names = sample_groups$groups)
#'
#' @export
#'

calc_aov_padj <- function(x,                          # a dataframe with the quantitative values
                          sample_names,               # a vector with the sample names similar to the header of the x dataframe. you can use partial matching
                          group_names,                # a vector with the group names with equal length of the sample_names vector
                          pAdj = "BH"){
  # check input -------------------------------------------------
  # check if the number of samples is equal with the number of groups
  print("Checking input ...")
  if (length(sample_names) != length(group_names)) {
    stop("vectors sample_names and group_names should have the same length", call. = FALSE)
  }

  # match the column names with the samples ------------------------------
  print("Matching column names with samples ...")
  # define two empty vectors one for the header and the second for the group
  index_of_sample <- vector(mode = "numeric", length = length(sample_names))
  ##### index_of_group <- vector(mode = "numeric", length = length(sample_names))

  # get two vectors one with the index of sample and one with the index of group
  for (i in seq_along(sample_names)) {
    if (length(grep(sample_names[i], names(x))) > 1) {
      stop(paste("More that one columns can match the sample name", sample_names[i],
                 ". Either make the names more specific, or trim more the initial dataframe"))
    }
    index_of_sample[i] <- grep(sample_names[i], names(x))
  }

  # perform ANOVA and extract p-values ----------------------------------
  print("Performing ANOVA and calculating p-values ...")
  # for each row, extract quantitative values based on index_of_sample vector
  #    transpose, merge with groups, perform ANOVA and extract p-value
  aov_pvalues <- rep(-1, time = nrow(x)) # -1 in the output means that the p-value was not calculated
  i <- 1
  # for every entry
  for (i in 1:nrow(x)) {
    # extract values
    values <- as.numeric(as.vector(x[i, index_of_sample]))
    aov_input <- data.frame(values = values, groups = group_names)
    # if there is an error pvalues will be labeld as -1
    model <- tryCatch(aov(formula = values ~ groups, data = aov_input), error = function(e) NULL)
    if (typeof(model) == "NULL") {
      pvalue <- -1
    } else {
      pvalue <- anova(model)$"Pr(>F)"[1]
    }
    aov_pvalues[i] <- pvalue
    i <- i + 1
  }
  # the processed data are saved in data dataframe
  data <- cbind(x, ANOVA_pvalue = aov_pvalues)
  data$ANOVA_pvalue[data$ANOVA_pvalue == "NaN"] <- -1
  data$ANOVA <- "measured"
  data$ANOVA[data$ANOVA_pvalue < 0] <- "excluded"
  print("--------------------------------")
  print("Entries measured/excluded from ANOVA")
  print(table(data$ANOVA))
  print("--------------------------------")

  # adjust p-values -----------------------------
  print("Correcting p-values ...")
  pAdjusted <- vector(length = nrow(data))
  data <- cbind(data, pAdjusted = pAdjusted)
  pAdj_label <- paste0("ANOVA_pval_", pAdj)
  names(data) <- gsub("pAdjusted", pAdj_label, names(data))

  # split data to excluded and measured
  data_excluded <- data[data$ANOVA == "excluded", ]
  data_measured <- data[data$ANOVA == "measured", ]

  ordered_data <- data_measured[order(data_measured$ANOVA_pvalue), ]
  ordered_data[[pAdj_label]] <- p.adjust(ordered_data$ANOVA_pvalue, method = pAdj, n = nrow(ordered_data))
  custom_breaks <- c(0, 0.05, 1)
  groups_vector <- cut(ordered_data$ANOVA_pval_BH, breaks = custom_breaks, include.lowest = TRUE)
  print("--------------------------------")
  print("Adjusted p-value frequency table")
  print(summary(groups_vector))
  print("--------------------------------")

  # merging tables and preparing output --------------------------------------
  print("Merging tables ...")
  output <- rbind(ordered_data, data_excluded)
  print("Done!")
  return(output)
}
