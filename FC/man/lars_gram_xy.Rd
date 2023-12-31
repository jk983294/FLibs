% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lars.R
\name{lars_gram_xy}
\alias{lars_gram_xy}
\title{lars_gram_xy}
\usage{
lars_gram_xy(
  Gram,
  Xy_vec,
  max.steps,
  type = c("lasso", "lar", "forward.stagewise", "stepwise"),
  trace = FALSE,
  normalize = TRUE,
  intercept = TRUE,
  eps = 1e-12,
  use.Gram = TRUE
)
}
\arguments{
\item{Gram}{= X^T * X}

\item{Xy_vec}{= X^T * y}

\item{max.steps}{how many features get selected}

\item{type}{lasso}

\item{trace}{default FALSE}

\item{normalize}{default TRUE}

\item{intercept}{default TRUE}

\item{eps}{default 1e-12}

\item{use.Gram}{default 1e-12}
}
\description{
lars based on gram and xy
}
