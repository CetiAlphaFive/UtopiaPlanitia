% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_quart.R, R/plot_rank.R
\name{rank_plot}
\alias{rank_plot}
\title{Rank Plot of Individual CATE Estimates}
\usage{
rank_plot(c.forest, show_ate_region = TRUE)

rank_plot(c.forest, show_ate_region = TRUE)
}
\arguments{
\item{c.forest}{A fitted causal forest model object from the \code{grf} package.}

\item{show_ate_region}{Logical indicating whether to show the ATE region (confidence interval). Default is TRUE.}
}
\value{
A ggplot2 object with the rank plot of individual CATE estimates.

A ggplot2 object with the rank plot of individual CATE estimates.
}
\description{
Generates a rank plot of individual CATE (Conditional Average Treatment Effect) estimates from a fitted causal forest model.

Generates a rank plot of individual CATE (Conditional Average Treatment Effect) estimates from a fitted causal forest model.
}
\examples{
\dontrun{
cf_model <- causal_forest(X, Y, W)
rank_plot(cf_model, show_ate_region = TRUE)
}
\dontrun{
cf_model <- causal_forest(X, Y, W)
rank_plot(cf_model, show_ate_region = TRUE)
}
}
