plot_norm_before_after <- function(before,
                                   after,
                                   raw_txt = "Raw log2 peptide intensities",
                                   norm_txt = "Normalized log2 peptide intensities (norm to mean)",
                                   xlab = "Log2 peptide intensity",
                                   ylab = "Density",
                                   title = "Peptide intensity distribution, before and after normalziation",
                                   show_legend = FALSE) {
  print("---------------------")
  print("Ploting inteisty distribution before and after normalization ...")
  print("---------------------")
  # convert to long format
  print("Reshaping data ...")
  before_df <- stack(before)
  before_df$group <- raw_txt
  after_df <- stack(after)
  after_df$group <- norm_txt
  graph_input <- rbind(before_df, after_df)
  graph_input$group <- factor(graph_input$group, levels = c(raw_txt, norm_txt))
  # make the plot
  head(graph_input)
  print("Preparing the plot ...")
  p <- ggplot(data = graph_input, aes(x = values, color = ind))
  p <- p + geom_density()
  p <- p + facet_wrap(~group)
  p <- p + theme_bw()
  p <- p + xlab(xlab)
  p <- p + ylab(ylab)
  p <- p + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  if (show_legend == FALSE) {
    p <- p + theme(legend.position = "none")
  }
  return(p)
}
