#' Calculate deuterioum uptake ration between the multiple time points over the last.
#'
#' @description Calculate deuterioum uptake ration between the multiple time points over the last,
#' by dividing the uptake value from each timepoint with the last time point of the series.
#' This division will be performed for each peptide of each protein.
#'
#' @param x A dataframe with the input values.
#' @param protein_col Name of the column containing the multiple proteins compared.
#' @param peptide_col Name of the column containing the peptide sequence.
#' @param uptake_col Name of the column containing the deuterium uptake values.
#' @param time_col Name of the column containing the timepoint of the measurement.
#'
#' @examples
#' x <- data
#' protein_col <- "mutant"
#' peptide_col <- "Sequence"
#' uptake_col <- "Uptake"
#' time_col <- "Exposure"
#'
#' time_series_ratios <- HDX_ltseries_uptake_ratio(x = data, protein_col = "mutant")
#'
#' @export

HDX_ltseries_uptake_ratio <- function(x,
                                      protein_col,
                                      peptide_col = "Sequence",
                                      uptake_col = "Uptake",
                                      time_col = "Exposure") {
    # make an empty vector to store uptake ratios
    uptake_ratios <- vector(mode = "numeric", length = nrow(x))
    # find unique proteins
    unique_proteins <- unique(x[[protein_col]])
    unique_peptides <- unique(x[[peptide_col]])
    for (i in seq_along(unique_proteins)) {
        print(paste("Processing", unique_proteins[i], "protein ..."))
        for (j in seq_along(unique_peptides)) {
            peptide_indexes <- which(x[[protein_col]] == unique_proteins[i] &
                                       x[[peptide_col]] == unique_peptides[j])
            filtered_input <- x[[uptake_col]][peptide_indexes]
            max_uptake <- max(x[peptide_indexes, time_col])
            uptake_ratios[peptide_indexes] <- filtered_input / max_uptake
      }
    }
    output <- cbind(x, uptake_ratios)
    return(output)
}
