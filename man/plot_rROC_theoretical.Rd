% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_rROC_theoretical.R
\name{plot_rROC_theoretical}
\alias{plot_rROC_theoretical}
\title{Plot (restricted) ROC curves for theoretic distributions}
\usage{
plot_rROC_theoretical(
  qnorm_positive,
  qnorm_negative,
  length.out_densityplot = 500,
  n_positive = 500,
  n_negative = 500,
  return_all = FALSE,
  ...
)
}
\arguments{
\item{qnorm_positive}{Quantile distribution function of positive class}

\item{qnorm_negative}{Quantile distribution function of negative class}

\item{length.out_densityplot}{Number of points to draw the density plot per curve}

\item{n_positive}{Number of positive samples.}

\item{n_negative}{Number of negative samples.}

\item{return_all}{If true, also return the rROC result, not only the plots}

\item{...}{Further arguments to \code{\link[=plot_density_rROC_empirical]{plot_density_rROC_empirical()}}}
}
\value{
If return_all: List of "data" and "rroc" result
Else: Only the "plots" from "rroc" result
}
\description{
Plot (restricted) ROC curves for theoretic distributions
}
\examples{

plot_rROC_theoretical(
    qnorm_positive = function(x) qnorm(x, mean = 1, sd = 1),
    qnorm_negative = function(x) qnorm(x, mean = 0, sd = 1)
)
}
