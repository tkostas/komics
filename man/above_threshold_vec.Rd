% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/above_threshold_vec.R
\name{above_threshold_vec}
\alias{above_threshold_vec}
\title{Above_threshold_vec}
\usage{
above_threshold_vec(x, y = 0)
}
\arguments{
\item{x}{A numeric vector.}

\item{y}{Threshold with default value 0.}
}
\description{
Reads a numeric vector and calculates how many elements are above a given threshold.
Output is a single element numeric vector with the number of observations.
}
\examples{
x <- seq(1:10)
y <- 5
above_threshold_vec(x, y)
}
