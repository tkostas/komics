#' Calculate sum of peptide intensities
#'
#' @description Reads a table containing a list of petides intensities and sums the peptide intensities
#' for each protein. Column containing protein identifiers should be included in the dataset.
#'
#' @param x Dataframe containing list of protein identifiers and intensity values for individual peptides.
#' Each row contains peptide intensities.
#' @param col_id Character string used to identify the columns containing peptide intensity.
#' @param protein_lab Column name of the column containing the protein identifiers.
#' @param custom_lab Custom character string to be added in front of every column name. Default value it “TIC_”.
#'
#' @examples
#' TIC <- calc_TIC(x, col_id = “normalized_int”, protein_lab = “Protein”)

calc_TIC <- function(x,
                     col_id,
                     protein_lab,
                     custom_lab = "TIC_") {
  print("---------------------")
  print("Calculating sum of peptide intensities ...")
  print("---------------------")

  # prepare tepm files
  print("Preparing temp files ...")
  intensity_indexes <- grep(col_id, colnames(x))
  intensity_indexes
  prot_vec <- c()
  prot_TIC <- vector()

  # split input
  print("Splitting data ...")
  if (length(x[[protein_lab]]) == 0) {
    stop("'protein_lab' argument should match exactly with the column containing protein identifiers. Correct value and try again.", call. = FALSE)
  }
  temp_list <- split(x = x, f = as.factor(x[[protein_lab]]))
  print(paste(length(temp_list), "proteins are included in the dataset ..."))
  # summing peptide intensities
  print("Summing peptide intensities ...")
  i <- 1
  sums <- matrix(nrow = length(temp_list), ncol = length(intensity_indexes))
  dim(sums)
  for (i in 1:length(temp_list)) {
    temp <- temp_list[[i]]
    prot_TIC <- purrr::map_dbl(temp[,intensity_indexes], sum, na.rm = TRUE)
    sums[i,] <- prot_TIC
    prot_vec <- c(prot_vec, names(temp_list[i]))
    i <- i + 1
  }

  # merging files
  print("Merging files ...")
  TIC <- as.data.frame(sums, row.names = FALSE)
  length(prot_vec)
  output <- as.data.frame(cbind(prot_vec, TIC))
  print(paste("Proccessed file has", nrow(output), "rows and", ncol(output), "columns ..."))
  # correcting names
  print("Correcting names ...")
  custom_lab <- "TIC_"
  prot_id <- "Protein Acc"
  old_names <- colnames(x[intensity_indexes])
  sample_id <- vector(length = length(old_names))
  i <- 1
  for (i in seq_along(old_names)) {
    sample_id[i] <- paste0(custom_lab, old_names[i])
  }

  new_names <- c(prot_id, sample_id)
  colnames(output) <- new_names
  head(output)


  # exporting table
  print("Done!")
  return(as.data.frame(output))

}
