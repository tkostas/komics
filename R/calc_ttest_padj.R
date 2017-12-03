#' Perform t-test and adjust p-values
#' @description Reads a dataframe, recognises protein groups, frequencies within each group,
#' performes t-test, calculates p-values, and adjusts p-value for multiple hypothesis testing error.
#' The starting dataframe can also contain columns with text. Experimental groups are identified based
#' on specific character string for each group. If missing values are converted to \code{NA}, they will
#' be omitted from the t-test. Minimum number of samples to be used in t-test can be defined by the user.
#' Default value is 3 samples per group. Two tailed t-test is used without assuming equal variance.
#' Equal variance can be selected by user using \code{t.test_var.equal} argument. P-values are
#' adjusted using "Benjamini-Hochberg" method "BH". To use a different method, change \code{pAdj_method}
#' parameter (see \code{?p.adjust} for more info. Results are saved in your working directory and can
#' be assigned to a variable for further processing.
#'
#' @param x Dataframe containing quantitative values.
#' @param id_groupA Identifier for the samples of the first study group.
#' @param id_groupB Identifier for the samples of the second study group.
#' @param min_A Minimum number of samples to be tested from group A.
#' @param min_B Minimum number of samples to be tested from group B.
#' @param t.test_var.equal Assumption for equal variance in t-test. Default is \code{FALSE}.
#' Set \code{TRUE} is you assume equal variance in the set.
#' @param pAdj_method Method for multiple hypothesis testing of the p-values. Default is "Benjamini-Hochberg"
#' method "BH". Run \code{?p.adjust} for more info.
#' @param output_name Name of the output file containing the results. This output will be saved in your
#' working directory. For this set the desired working directory before running the function. Also
#' the output will be printed, so you can save it in a variable.
#'
#' @examples
#' calc_ttest_padj(norm_data, id_groupA = "log2_disease", id_groupB = "log2_controls")
#' @export

calc_ttest_padj <- function(x,
                            id_groupA = "_groupA",
                            id_groupB = "_groupB",
                            min_A = 3,
                            min_B = 3,
                            t.test_var.equal = FALSE,
                            pAdj_method = "BH",
                            output_name = "processed p-values.txt") {
  # Step A. Identify groups and prepare data
  print("Checking dataset ...")
  groupA <- grep(id_groupA, colnames(x))
  if (length(groupA) == 0) {
    stop(paste(id_groupA, "is not present in the columns of group A. Check column names."), call. = FALSE)
  } else if (length(groupA) < min_A) {
    stop(paste("Not enough samples found for group A (min is", min_A, ")"), call. = FALSE)
  } else {
    print(paste(length(groupA), "Samples assigned to group A"))
  }
  groupB <- grep(id_groupB, colnames(x))
  if (length(groupB) == 0) {
    stop(paste(id_groupB, "is not present in the columns of group B. Check column names."), call. = FALSE)
  } else if (length(groupB) < min_B) {
    stop(paste("Not enough samples found for group B (min is", min_B, ")"), call. = FALSE)
  } else {
    print(paste(length(groupB), "Samples assigned to group B"))
  }

  # Step B. Calculate frequency for each group and rank data
  ## make one column for each group and write the number of samples with more than min_A/B identifications

  x$frequencyGroupA <- apply(x[,groupA], 1, function(x)(length((which(!is.na(x))))))
  x$frequencyGroupB <- apply(x[,groupB], 1, function(x)(length((which(!is.na(x))))))

  ## make one column ranking each protein based on the number of samples that this protein is identified
  print("Spliting groups ...")
  i <- 1
  AvsB <- vector(length = nrow(x))
  for (i in i:nrow(x)) {
    if (x$frequencyGroupA[i] >= min_A & x$frequencyGroupB[i] >= min_B) {
      AvsB[i] <- "both in A and B"
    }else if (x$frequencyGroupA[i] >= min_A & x$frequencyGroupB[i] < min_B) {
      AvsB[i] <- "only A"
    }else if (x$frequencyGroupA[i] < min_A & x$frequencyGroupB[i] >= min_B) {
      AvsB[i] <- "only B"
    }else{
      AvsB[i] <- "neither A or B"
    }
  }
  x$AvsB <- AvsB

  ## subset the data and use only proteins with f>= min A/B for t-test
  compareData <- subset(x, AvsB == "both in A and B")
  restData <- subset(x, AvsB != "both in A and B")


  ## keep the rest data for c-bind after t-test (add in p.Value and p.adjBH column NA)
  restData$pValue <- rep(NA, times = nrow(restData))
  restData$p.adjBH <- rep(NA, times = nrow(restData))

  # Step C. Subset and perform t-test
  print("Running t-test ...")
  i <- 1
  pVal <- c()
  for (i in 1:nrow(compareData)) {
    A <- compareData[i, groupA]
    B <- compareData[i, groupB]
    TT <- t.test(A, B, na.action = na.omit, var.equal = t.test_var.equal)
    pVal <- c(pVal, TT$p.value)
  }
  compareData$pValue <- pVal


  # Step D. Adjust p-values (BH) and write output
  print(paste("Adjusting p-values using", pAdj_method, "method ..."))
  padjust_df <- compareData[order(compareData$pValue),]
  padjust_df$p.adjBH <- p.adjust(padjust_df$pValue, method = pAdj_method, n = length(padjust_df$pValue)) #select method

  ## row bind
  output_table <- rbind(padjust_df, restData)
  print(paste("Saving output in", output_name, "in the defined working directory ..."))
  ##  Write output
  write.table(output_table, file = output_name, row.names = FALSE, sep = "\t")
  print("Done!")
  return(output_table)
}
