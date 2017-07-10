#' Create volcano plot
#' @description Reads a data frame and plots fold difference and p-value in a volcano plot.
#' P-values can be calculated using \code{calc_ttest_padj} and fold change (log2 of fold change)
#' using \code{calc_fold_change}. Column names for p-value and \code{log2_fold} difference is
#' matched based on character matched with \code{p_val} and \code{fold_change} arguments.
#' Thresholds can be modified using \code{fold_threshold} and \code{p_val_threshold} arguments.
#' Plot parameters are added in the \code{plot_config} list. Protein annotation (e.g. gene names)
#' can be added. If annotation is not included in the data frame, you can use \code{append_cols_df}
#' function to add extra columns in your dataset and then continue with plotting. The processed
#' dataset of \code{plot_volc} function can be saved in an object (see example).
#'
#' @param x Data frame containing p-values and calculated fold_change. If fold change is not
#' calculated use \code{calc_fold_change} function first and use the output.
#' @param p_val Character string for the identification of the column containing p-values.
#' Only one column should match.
#' @param fold_change Character string for the identification of the column containing
#' log2_fold change values. Only one column should match.
#' @param fold_threshold Threshold for fold difference. Default is 2. You don’t have
#' to calculate the log2 value of the threshold.
#' @param p_val_threshold Threshold for t-test p-value. Default is 0.05.
#' @param plot_config List of plot parameters (see usage):\cr
#'    \code{col}: vector containing the colors for proteins with significant abundance\cr
#'     in group A, B and non-significant.
#'    \code{point_size}: Point size.\cr
#'    \code{xlim}: limits for the x-axis.\cr
#'    \code{ylim}: limits for the y-axis.\cr
#'    \code{xlab}: Title for the x axis. Default is  "Log2 fold change A/B".\cr
#'    \code{ylab}: Title for the y-axis. Default is "-log p-value".\cr
#'    \code{title}: Plot title. Default is "Differentially abundant proteins between groups A and B".\cr
#'    \code{show_labs}: Logical with default value FALSE. If switched to TRUE text annotation
#'    defined by the labs_id element (see below) will be added in differentially abundant points.\cr
#'    \code{lab_lines}: Logical with default value FALSE. If switched to TRUE, connecting lines
#'    between individual points and labels will appear. Uses function from ggrepel package.\cr
#'    It because of the complexity due to labeling, it is advised first to see the number
#'    of differentially abundant proteins and then replot the graph with text annotation.\cr
#'    \code{labs_size}: Size of the text annotation. Default is 2.\cr
#'    \code{labs_id}: Column containing text annotation, like gene names. Default value is
#'    “Gene.names...primary..”. Gene names can be downloaded from Uniprot and merged
#'    using \code{append_cols_df} function.
#'
#' @examples p_val <- "pValue"
#' fold_change <- "log2_fold_change_AB"
#' plot_config <- list(col = c("blue", "red", "grey"),
#'                    point_size = 2,
#'                    xlim = c(-4, 4),
#'                    ylim = c(0, NA),
#'                    xlab = "Log2 fold change A/B",
#'                    ylab = "-log p-value",
#'                    title = "DA proteins between groups A and B",
#'                    show_labs = TRUE,
#'                    lab_lines = TRUE,
#'                    labs_size = 2,
#'                    labs_id = "Gene.names...primary..")
#'
#' my_graph_data <- plot_volc(x = data,
#'                           p_val = p_val,
#'                           fold_change = fold_change,
#'                           fold_threshold = 1.5,
#'                           p_val_threshold = 0.05,
#'                           plot_config = plot_config)
#'
#' write.csv(my_graph_data, file = “my_graph_data.csv”, row.names = FALSE)
#' @export



plot_volc <- function(x,
                      p_val,
                      fold_change,
                      fold_threshold = 2,
                      p_val_threshold = 0.05,
                      plot_config = list(col = c("blue", "red", "grey"),
                                         point_size = 1,
                                         xlim = c(-4, 4),
                                         ylim = c(0, NA),
                                         xlab = "Log2 fold change A/B",
                                         ylab = "-log p-value",
                                         title = "Differentially abundant proteins between groups A and B",
                                         show_labs = FALSE,
                                         lab_lines = FALSE,
                                         labs_size = 2,
                                         labs_id = "Gene.names...primary..")){
  # find p-values column
  print("Identifying p-value ...")
  indx_pval <- get_single_col_index_df(x, p_val)
  col_pval <- colnames(x)[indx_pval]
  print(paste0("'", col_pval, "' identified for containing p-values."))

  # find fold_change column
  print("Identifying fold_change ...")
  indx_fc <- get_single_col_index_df(x, fold_change)
  col_fc <- colnames(x)[indx_fc]
  print(paste0("'", col_fc, "' identified for containing fold change for A/B (log2)."))

  # assign categories based on custom thresholds
  print("Identifying differentially abundant proteins ...")
  fc_threshold <- log(fold_threshold, 2)
  abundance_group <- vector(length = ncol(x))
  # significant abundant in A
  sig_A <- x[,col_pval] < p_val_threshold & x[,col_fc] >= fc_threshold
  # sig_A
  sig_B <- x[,col_pval] < p_val_threshold & x[,col_fc] <= -fc_threshold
  # sig_B
  NS <- x[,col_pval] > p_val_threshold | (x[,col_fc] <= fc_threshold & x[,col_fc] >= -fc_threshold)
  # NS

  # assign significans group
  abundance_group[sig_A] <- "Significant in A"
  abundance_group[sig_B] <- "Significant in B"
  abundance_group[NS] <- "non-significant"
  # abundance_group

  x$abundance_group <- abundance_group
  x$minus_log_pval <- -log10(x[,col_pval])

  # keep only the values to be plotted
  print("Editing labels ...")
  AorB <- x$abundance_group == "Significant in A" | x$abundance_group == "Significant in B"
  if (plot_config$show_labs == TRUE) {
    x$text_labs <- x[,paste(plot_config$labs_id)]
  }
  x$text_labs[!AorB] <- NA

  print("Trimming data ...")
  sub_data <- subset(x, subset = x$abundance_group == "Significant in A" |
                       x$abundance_group == "Significant in B" |
                       x$abundance_group == "non-significant")


print(paste(plot_config$show_labs))
print(paste(plot_config$lab_lines))

  print("Preparing the plot ...")
  p <- ggplot(sub_data, aes(get(col_fc), minus_log_pval, color = abundance_group))
  p <- p + theme_bw()
  p <- p + geom_point(aes(colour = factor(abundance_group)), size = plot_config$point_size)
  p <- p + scale_colour_manual(values = c(`Significant in A` = plot_config$col[1],
                                          `Significant in B` = plot_config$col[2],
                                          `non-significant` = plot_config$col[3]))
  p <- p + xlim(limits = plot_config$xlim) + ylim(limits = plot_config$ylim)
  p <- p + xlab(plot_config$xlab) + ylab(plot_config$ylab) + ggtitle(plot_config$title)
  p <- p + geom_hline(linetype = "dashed", yintercept = -log10(p_val_threshold))
  p <- p + geom_vline(linetype = "dashed", xintercept = fc_threshold)
  p <- p + geom_vline(linetype = "dashed", xintercept = -fc_threshold)
  if (plot_config$show_labs == TRUE & plot_config$lab_lines == FALSE) {
    p <- p + geom_text(aes(label = sub_data$text_labs), size = plot_config$labs_size)
  }
  if (plot_config$lab_lines == TRUE & plot_config$show_labs == TRUE) {
    p <- p + geom_text_repel(aes(label = sub_data$text_labs), size = plot_config$labs_size) #from ggrepel package
  }
  print(p)
  print("Modified dataset is saved into object.")
  return(x)
}
