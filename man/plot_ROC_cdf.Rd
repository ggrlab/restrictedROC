% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_ROC_cdf.R
\name{plot_ROC_cdf}
\alias{plot_ROC_cdf}
\title{ROC curve for given cumulative distribution functions}
\usage{
plot_ROC_cdf(
  cdf_positive,
  cdf_negative,
  xmin = -1,
  xmax = 1,
  length.out = 1000
)
}
\arguments{
\item{cdf_positive}{Cumulative distribution function for the positive class}

\item{cdf_negative}{Cumulative distribution function for the negative class}

\item{xmin}{Minimum value of simulated points for the plots from the distributions.
\code{length.out} points will be generated between \verb{[xmin, xmax]}.
PURELY VISUAL, ROC curves are calculated based on the actual distributions!}

\item{xmax}{Maximum value of simulated points for the plots from the distributions.
\code{length.out} points will be generated between \verb{[xmin, xmax]}.
PURELY VISUAL, ROC curves are calculated based on the actual distributions!}

\item{length.out}{The number of simulated points for the plots from the distributions.
\code{length.out} points will be generated between \verb{[xmin, xmax]}.
PURELY VISUAL, ROC curves are calculated based on the actual distributions!}
}
\value{
ggplot ROC-curve
}
\description{
ROC curve for given cumulative distribution functions
}
\examples{
plot_ROC_cdf(
    cdf_positive = function(x) pnorm(x, mean = 1, sd = 1),
    cdf_negative = function(x) pnorm(x, mean = 0, sd = 1),
    xmin = -5, xmax = 15, length.out = 2500
)
plot_ROC_cdf(
    cdf_positive = function(x) pnorm(x, mean = 1, sd = 1.5),
    cdf_negative = function(x) pnorm(x, mean = 0, sd = 1),
    xmin = -5, xmax = 15, length.out = 2500
)
plot_ROC_cdf(
    cdf_positive = function(x) pnorm(x, mean = 1, sd = 2.5),
    cdf_negative = function(x) pnorm(x, mean = 0, sd = 1),
    xmin = -5, xmax = 15, length.out = 2500
)
}
