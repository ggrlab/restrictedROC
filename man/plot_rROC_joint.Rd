% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_rROC.R
\name{plot_rROC_joint}
\alias{plot_rROC_joint}
\title{Plot ROC curve with rzAUC values inside}
\usage{
plot_rROC_joint(roc_part, df, part, col_rzAUC)
}
\arguments{
\item{roc_part}{data.frame with true positive rate (tpr) and false positive rate (fpr)
to plot the ROC curve}

\item{df}{data.frame from single_rROC$perf where

\if{html}{\out{<div class="sourceCode">}}\preformatted{    single_rROC <- simple_rROC_interpret(simple_rROC(
        response = aSAH$outcome,
        predictor = aSAH$ndka
    ))
}\if{html}{\out{</div>}}}

\item{part}{\itemize{
\item "high" (for markerHIGH part, bottom-left part of ROC curve)
\item "low" (for markerLOW part, top-right part of ROC curve)
}}

\item{col_rzAUC}{Color of the rzAUC points}
}
\value{
ggplot object with two axes:
1. Usual ROC curve
2. The rzAUC in the same plot on the second axis
}
\description{
Plot ROC curve with rzAUC values inside
}
\examples{
# See plot_rROC()
}
