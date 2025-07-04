% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing_functions.R
\name{standardize_date_format}
\alias{standardize_date_format}
\title{Standardize Date Format in Data Frame}
\usage{
standardize_date_format(df)
}
\arguments{
\item{df}{A data frame or tibble that includes a `date` column as character or factor.}
}
\value{
The input data frame with the `date` column converted to `zoo::yearmon` format.
}
\description{
Converts the `date` column of a data frame to `yearmon` format based on
common date string formats.   Supports "mm-dd-YYYY" and "YYYY-mm" formats.
}
