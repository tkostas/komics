#' Plot frequencies in a barplot (use in combination with above_threshold_df)
#'
#' @description Reads a list of frequencies calculated for different groups (see above_threshold_df)
#' and makes a barplot for the mean between groups or for the individual measurements. Use to plot
#' number of identified peptides/proteins per group or per sample.
#'
#' @return Returns a graph.
#'
#' @param x Input list. Each element of the list has a title that will be used to define the group
#' and to order the bars in the bar-graph. Every element of this list is a vector containing numeric
#' values (e.g. number of proteins per experiment). The names of this vector are the experiments.
#' Use as input the output of the above_threshold_df function.
#' @param compare Character vector that can be "groups" or "experiments", either to summarize
#' values for groups or show the values for individual experiments. If "groups" are selected,
#' the mean value +- SEM is plotted.
#' @param ylim Numeric vector, defining min and max value for the y axis.
#'
#' @examples
#' # first calculate the frequencies using the above_threshold_df function
#' place the group names in the correct order before calculating frequencies
#' Bmp4 <- "Bmp4"
#' Ctrl <- "Ctrl"
#' group_ids <- list(Bmp4 = Bmp4,
#'                   Ctrl = Ctrl)
#'
#' number_of_ids <- above_threshold_df(data, group_ids = group_ids, threshold = 0)
#'
#' # use the list with frequencies as input to make the barplots
#'
#' # summarize identifications per 'group'
#' plot_bar_list(number_of_ids, "groups")
#'
#' # plot frequencies per 'experiment'
#' plot_bar_list(number_of_ids, "experiments")
#' @export


plot_bar_list <- function(x, compare = "groups", ylim = c(0, NA)) {
  # check if input is list
  if (!is.list(x)) {
    stop("Error! Input data should be in a list format. See help for more info.", call. = FALSE)
  }
  # collect list values and prepare data in long format
  print("Reading input ...")
  values <- c()
  experiment <- c()
  group <- c()

  for (i in seq_along(x)) {
    values <- c(values, x[[i]])
    experiment <- c(experiment, names(x[[i]]))
    observations <- length(x[[i]])
    group <- c(group, rep(names(x[i]), times = observations))
  }
  long_data <- data.frame(values = values, experiment = experiment, group = group)
  long_data
  if (compare == "experiments") {
    print("Preparing graph for experiments ...")
    p <- ggplot(long_data, aes(experiment, values, fill = group))
    p <- p + theme_bw() + geom_bar(stat = "identity") + ylim(limits = ylim)
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    p <- p + scale_x_discrete(limits = long_data$experiment)
    p <- p + geom_text(aes(label = values), vjust = -0.25)
    print(p)
  } else if (compare == "groups") {
    print("Preparing graph for groups ...")
    p <- ggplot(long_data, aes(group, values, fill = group))
    p <- p + theme_bw() + stat_summary(fun.y = "mean", geom = "bar") + ylim(limits = ylim)
    p <- p + stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0.4)
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    p <- p + scale_x_discrete(limits = unique(long_data$groups))
    print(p)
  } else {
    print("Select to compare either groups or experiments and repeat.")
  }

}
