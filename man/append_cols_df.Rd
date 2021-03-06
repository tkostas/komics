% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/append_cols_df.R
\name{append_cols_df}
\alias{append_cols_df}
\title{Append extra columns to a dataframe}
\usage{
append_cols_df(x, y, key_x, key_y, append_y)
}
\arguments{
\item{x}{Main working data frame. \code{NA} will be added in the rows on \code{x} that do not match with \code{y}.}

\item{y}{Secondary data frame containing columns to be added in the main one. Columns that do not match with x will be excluded.}

\item{key_x}{Character string describing key column of the first data frame. You can use partial matching.}

\item{key_y}{Character string describing key column of the second data frame. You can use partial matching.}

\item{append_y}{Character vector containing strings that match the columns to be added in the main data frame. You can use also partial matching.}
}
\description{
Reads two data frames \code{x, y} and based on the user defined key
columns \code{key_x, key_y}, add the columns described by \code{append_y} to the \code{x} table.
}
\examples{
add_me <- colnames(y)[c(4, 10)]
ap <- append_cols_df(x = k3,
                  y = y,
                  key_x = "Protein A",
                  key_y = "Entry$",
                  append_y = add_me)
}
