#' Plot keywords after enrichment test as lines
#' @description Reads the output of \code{link[komics]{kwd_enrichment_multi}}, along with
#'         a vector with the categories names and plots a linegraph of the keyword ratio between
#'         test group and the background.
#'         You don't have any options to modify the appearence of the graph, in this version.
#'
#' @param x A list containing the keyword enrichment output from \code{link[komics]{kwd_enrichment_multi}}
#'        function. You remove elements of this list, as long as you don't change the column names.
#' @param group_vec A vector containing the labels of each group. Should have the same length
#'        with the \code{x} argument.
#' @param save_output Defalut value is FALSE. If a character string is added, the output will be saved
#'        in your working directory with the specified name, as tab delimited file.
#' @param x_discrete Default valus is FALSE. Use the \code{kwd_vector } from \code{\link[komics]{kwd_enrichment_multi}}
#'        to order the \code{x} axis, accordingly.
#'
#' @examples
#' \donttest{
#'
#' plot_kwd_lines(x = kwd_enrichment_multi_output,
#'                group_vec = categories_vector)
#' }
#' @export


plot_kwd_lines <- function(x,
                           group_vec,
                           save_output = FALSE,
                           x_discrete = FALSE){
  for (i in seq_along(group_vec)){
    kwd_table <- x[[i]]
    kwd_table$group <- group_vec[i]
    if (i == 1) {
      output <- kwd_table
    } else {
      output <- rbind(output, kwd_table)
    }
  }
  if (is.character(save_output)){
    print(paste0("Saving output in the working directory as '", save_output, ".txt'."))
    write.table(output, file = paste0(save_output, ".txt"), row.names = FALSE, sep = "\t")
  }
  p <- ggplot(output, aes(keyword, ratio_test_group, col = group, group = group)) +
    geom_line(size = 2) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    if (x_discrete != FALSE) {
      p + scale_x_discrete(limits = x_discrete)
    } else {
      p
    }
}
