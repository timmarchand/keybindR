#' Bind Weighted Log Odds
#'
#' Computes the weighted log odds ratio for each token and adds it as a new column.
#' This metric adjusts the log odds by weighting rare events to avoid overestimation, following the method
#' described by Monroe et al. (2008). It can also compute the uninformative prior to account for unknown vocabulary.
#'
#' @param tbl A data frame containing token counts.
#' @param target_corpus The name of the column containing counts from the target corpus (as a string).
#' @param reference_corpus The name of the column containing counts from the reference corpus (as a string).
#' @param uninformative Logical; if \code{TRUE}, applies the uninformative prior to smooth the log odds for rare tokens.
#'
#' @details
#' The weighted log odds ratio is calculated as:
#' \deqn{\delta_{wi} = \log\left(\frac{a + \alpha_w}{c + \beta_w}\right) - \log\left(\frac{b + \alpha_w}{d + \beta_w}\right)}
#' where:
#' \itemize{
#'   \item \eqn{a} is the count of the token in the target corpus.
#'   \item \eqn{b} is the count of the token in the reference corpus.
#'   \item \eqn{c} is the count of the token not present in the target corpus.
#'   \item \eqn{d} is the count of the token not present in the reference corpus.
#'   \item \eqn{\alpha_w} and \eqn{\beta_w} are smoothing parameters. If \code{uninformative = TRUE}, these are set to 1.
#' }
#'
#' The function also calculates the z-score (\eqn{z_{wi}}), which is the weighted log odds divided by the standard error:
#' \deqn{\sigma^2_{wi} = \frac{1}{a + \alpha_w} + \frac{1}{b + \alpha_w}}
#' \deqn{z_{wi} = \frac{\delta_{wi}}{\sqrt{\sigma^2_{wi}}}}
#'
#' If the necessary contingency table columns (`a`, `b`, `c`, `d`, `n1`, `n2`) are not present in the data frame,
#' and `target_corpus` and `reference_corpus` are specified, the function will compute the contingency table.
#' If the columns are missing and the corpus columns are not specified, the function will stop with an error.
#'
#' @return A data frame with the original columns and two additional columns: `log_odds_weighted` (the weighted log odds)
#' and `z_score` (the corresponding z-score).
#'
#' @examples
#' library(dplyr)
#'
#' # Sample data
#' df <- data.frame(
#'   token = c("word1", "word2", "word3"),
#'   target_count = c(30, 50, 20),
#'   reference_count = c(40, 30, 10)
#' )
#'
#' # Compute weighted log odds with an uninformative prior
#' df_with_logOdds_weighted <- bind_logOdds_weighted(
#'   tbl = df,
#'   target_corpus = "target_count",
#'   reference_corpus = "reference_count",
#'   uninformative = TRUE
#' )
#'
#' # View the result
#' print(df_with_logOdds_weighted)
#'
#' @export
bind_logOdds_weighted <- function(tbl, target_corpus = NULL, reference_corpus = NULL, uninformative = FALSE) {
  # Check if contingency table columns are present
  required_cols <- c("a", "b", "n1", "n2")
  if (!all(required_cols %in% names(tbl))) {
    if (!is.null(target_corpus) && !is.null(reference_corpus)) {
      tbl <- bind_contingency_table(tbl, target_corpus, reference_corpus)
      message("Contingency table columns created and added to the data frame.")
    } else {
      stop("Contingency table columns are missing and target_corpus and reference_corpus are not specified.")
    }
  }

  # Calculate the contingency table values
  y_wi <- as.numeric(tbl$a)
  y_wj <- as.numeric(tbl$b)
  y_w <- y_wi + y_wj
  n_i <- as.numeric(tbl$n1)
  n_j <- as.numeric(tbl$n2)
  n <- n_i + n_j

  # Use uninformative priors if specified
  if (uninformative) {
    alpha_w <- 1
    beta_w <- 1
  } else {
    total_vocab <- nrow(tbl)
    y_w_total <- sum(y_w, na.rm = TRUE)
    alpha_w <- (y_w + 1) / (y_w_total + total_vocab)
    beta_w <- 1
  }

  # Calculate adjusted counts with priors
  a_adj <- y_wi + alpha_w
  b_adj <- y_wj + alpha_w
  c_adj <- (n_i - y_wi) + beta_w
  d_adj <- (n_j - y_wj) + beta_w

  # Calculate weighted log odds and z-score
  delta_wi <- log(a_adj / c_adj) - log(b_adj / d_adj)
  sigma2_wi <- 1 / a_adj + 1 / b_adj
  z_wi <- delta_wi / sqrt(sigma2_wi)

  # Add results to the data frame
  tbl_with_logOdds_weighted <- tbl %>%
    mutate(
      log_odds_weighted = delta_wi,
      z_score = z_wi
    )

  return(tbl_with_logOdds_weighted)
}
