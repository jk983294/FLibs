% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FM_stats.R
\name{percentile}
\alias{percentile}
\title{percentile}
\usage{
percentile(x, ties.method = "max", ...)
}
\arguments{
\item{x}{vec}

\item{ties.method}{see data.table::frank(ties.method=)}

\item{...}{additional parameters passed to data.table::frank}
}
\description{
Percentile rankings rescales a numeric vector to ⁠[0, 1]⁠ according to the ranking. If all values are equal, each is mapped to 0.5.
}
