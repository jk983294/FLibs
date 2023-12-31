% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fc.R
\name{tsplot}
\alias{tsplot}
\title{tsplot}
\usage{
tsplot(
  x,
  y = NULL,
  map = identity,
  date = "cob",
  time = "ticktime",
  benchmark = NULL,
  type = "p",
  pch = ".",
  cex = 2,
  lty = 1,
  col = NULL,
  xlab = NULL,
  ylab = NULL,
  main = NULL,
  date_label = NULL,
  time_label = NULL,
  margin_label = NULL,
  legend_label = y,
  ...,
  year_lines = FALSE,
  month_lines = FALSE,
  day_lines = FALSE,
  hour_lines = FALSE,
  vlines = NULL,
  hlines = NULL,
  legend = NULL
)
}
\arguments{
\item{x}{a data.table with date/time key}

\item{y}{selector of columns in x to plot}

\item{...}{additional parameters passed to matplot}
}
\description{
Plot time series
}
