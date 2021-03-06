% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/col_exists.R
\name{col_exists}
\alias{col_exists}
\title{Checks if column names exist in columns of dataframe.}
\usage{
col_exists(df, colname)
}
\arguments{
\item{df}{data frame to be tested}

\item{colname}{a vector of character strings to check}
}
\value{
True if found, false if not
}
\description{
This is a private function for dfcompare.
}
\details{
It is currently used to detect if the specified keys are in the data frame.
If not found, it prints and error message indicating the missing columns.
}
