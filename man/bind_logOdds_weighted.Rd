% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bind_logOdds_weighted.R
\name{bind_logOdds_weighted}
\alias{bind_logOdds_weighted}
\title{Bind Weighted Log Odds}
\usage{
bind_logOdds_weighted(
  tbl,
  target_corpus = NULL,
  reference_corpus = NULL,
  uninformative = FALSE
)
}
\arguments{
\item{tbl}{A data frame containing token counts.}

\item{target_corpus}{The name of the column containing counts from the target corpus (as a string).}

\item{reference_corpus}{The name of the column containing counts from the reference corpus (as a string).}

\item{uninformative}{Logical; if \code{TRUE}, applies the uninformative prior to smooth the log odds for rare tokens.}
}
\value{
A data frame with the original columns and two additional columns: \code{log_odds_weighted} (the weighted log odds)
and \code{z_score} (the corresponding z-score).
}
\description{
Computes the weighted log odds ratio for each token and adds it as a new column.
This metric adjusts the log odds by weighting rare events to avoid overestimation, following the method
described by Monroe et al. (2008). It can also compute the uninformative prior to account for unknown vocabulary.
}
\details{
The weighted log odds ratio is calculated as:
\deqn{\delta_{wi} = \log\left(\frac{a + \alpha_w}{c + \beta_w}\right) - \log\left(\frac{b + \alpha_w}{d + \beta_w}\right)}
where:
\itemize{
\item \eqn{a} is the count of the token in the target corpus.
\item \eqn{b} is the count of the token in the reference corpus.
\item \eqn{c} is the count of the token not present in the target corpus.
\item \eqn{d} is the count of the token not present in the reference corpus.
\item \eqn{\alpha_w} and \eqn{\beta_w} are smoothing parameters. If \code{uninformative = TRUE}, these are set to 1.
}

The function also calculates the z-score (\eqn{z_{wi}}), which is the weighted log odds divided by the standard error:
\deqn{\sigma^2_{wi} = \frac{1}{a + \alpha_w} + \frac{1}{b + \alpha_w}}
\deqn{z_{wi} = \frac{\delta_{wi}}{\sqrt{\sigma^2_{wi}}}}

If the necessary contingency table columns (\code{a}, \code{b}, \code{c}, \code{d}, \code{n1}, \code{n2}) are not present in the data frame,
and \code{target_corpus} and \code{reference_corpus} are specified, the function will compute the contingency table.
If the columns are missing and the corpus columns are not specified, the function will stop with an error.
}
\examples{
library(dplyr)

# Sample data
df <- data.frame(
  token = c("word1", "word2", "word3"),
  target_count = c(30, 50, 20),
  reference_count = c(40, 30, 10)
)

# Compute weighted log odds with an uninformative prior
df_with_logOdds_weighted <- bind_logOdds_weighted(
  tbl = df,
  target_corpus = "target_count",
  reference_corpus = "reference_count",
  uninformative = TRUE
)

# View the result
print(df_with_logOdds_weighted)

}
