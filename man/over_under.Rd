% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/over_under.R
\name{over_under}
\alias{over_under}
\title{Label each row based on specific rules}
\usage{
over_under(x, thresholds = list(over = list(pval = ">0.7", group = "B"), under
  = list(pval = "<0.4", group = "A")), labels = list(over = "increased", under
  = "decreased", colname = "Difference"))
}
\arguments{
\item{x}{A data.frame containing the values to be tested.}

\item{thresholds}{A list of lists (see examples). The 1st level of elements in this list describes
the category of labeling. The 2nd level, contains the rules that need to be TRUE.
The \strong{names of the 2nd level lists should match with the columns that will be used for
the comparison} (see example). The values in the 2nd level list can be either text
or can contain a symbol for comparison and a number (e.g. "> 0.5"). Symbols recognized,
include '>', '<' and '='.}

\item{labels}{A list with the corresponding labels. If the rules from the thresholds list are TRUE,
the corresponding value of the labels list will be added in the data.frame. Keep the
same order with the thresholds. If the 1st list of thresholds is positive, the 1st label
will be added. The last entry in the labels list should be labeled 'colname' and contains
the column name of the new column that will be added. See example for more info.}
}
\description{
Reads a dataframe 'x' and based on the rules described in the 'thresholds' list
  will create a new column and add the corresponding label. Keep the same order in the thresholds
  and labels list.
}
\examples{
x <- data.frame(pval = seq(from = 0.1, to = 1, by = 0.1),
                group = c(rep("A", times = 5), rep("B", times = 5)))

rules_list <- list(over = list(pval = ">0.7",
                               group = "B"),
                   under = list(pval = "<0.4",
                                group = "A"))

labels_list <- list(over = "increased",
                    under = "decreased",
                    colname = "Difference")

over_under(x, thresholds = rules_list, labels = labels_list)
}
