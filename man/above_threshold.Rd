% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/above_threshold.R
\name{above_threshold}
\alias{above_threshold}
\title{Calculate number of observations above a certain threshold}
\usage{
above_threshold(x, apply_to = "columns", group_ids = NULL, threshold = 0)
}
\arguments{
\item{x}{A dataframe. You can select which columns to be used using the group_ids argument.}

\item{apply_to}{Character vector. Calculate frequency for "columns", or "rows". Default value is "columns".}

\item{group_ids}{List of group names and group identifiers (see example). If all the columns are used as one group,
skip this argument. To specific columns for one or more groups define a list in which the name of each list element
will be the group name and every element will contain a character vector that matches with the column names of the specific group.
Matching of the columns can be partial (e.g. ctrl_ to select multiple columns containing the "ctrl_" character string)}

\item{threshold}{The minimum value to calculate frequency. Default is 0.}
}
\value{
Output is a list and each element is a vector containing
the sum of observations that are above the threshold. Each element has the same name as it was defined in the
group_ids list, and omitting naming the elements of the list will return an error. This way you can calculate
frequencies for different groups in parallel.
}
\description{
Reads a data frame and calculates the number of observations that are above a certain threshold value.
Frequency is calculated for columns or rows. Groups can be assigned in the group_ids argument, by passing
a list with the corresponding info (see arguments).
}
\examples{
treatment_group <- c("Tretment", "treated")
control_group <- c("ctrl", "control")
group_ids <- list(treatment = treatement_group,
                  control = control_group)

above_threshold(data, group_ids = group_ids, threshold = 0)
}
