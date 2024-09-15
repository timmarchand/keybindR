#' Bind Log Odds Ratio
#'
#' Computes the log odds ratio for each token and adds it as a new column. Log odds ratio
#' measures the association between a token and the target corpus compared to the reference corpus.
#'
#' @param tbl A data frame containing token counts.
#' @param target_corpus The name of the column containing counts from the target corpus (as a string).
#' @param reference_corpus The name of the column containing counts from the reference corpus (as a string).
#'
#' @details
#' The log odds ratio is calculated as:
#' \deqn{log\_odds = \log\left(\frac{a \times d}{b \times c}\right)}
#' where:
#' \itemize{
#'   \item \eqn{a} is the count of the token in the target corpus.
#'   \item \eqn{b} is the count of the token in the reference corpus.
#'   \item \eqn{c} is the count of tokens in the target corpus not including the token.
#'   \item \eqn{d} is the count of tokens in the reference corpus not including the token.
#' }
#'
#' If the necessary contingency table columns (`a`, `b`, `c`, `d`) are not present in the data frame,
#' and `target_corpus` and `reference_corpus` are specified, the function will compute the contingency table.
#' If the columns are missing and the corpus columns are not specified, the function will stop with an error.
#'
#' @return A data frame with the original columns and an additional `log_odds` column containing the log odds ratio.
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
#' # Compute log odds
#' df_with_logOdds <- bind_logOdds(
#'   tbl = df,
#'   target_corpus = "target_count",
#'   reference_corpus = "reference_count"
#' )
#'
#' # View the result
#' print(df_with_logOdds)
#'
#' @export
bind_logOdds <- function(tbl, target_corpus = NULL, reference_corpus = NULL) {
  # Check if contingency table columns are present
  required_cols <- c("a", "b", "c", "d")
  if (!all(required_cols %in% names(tbl))) {
    if (!is.null(target_corpus) && !is.null(reference_corpus)) {
      tbl <- bind_contingency_table(tbl, target_corpus, reference_corpus)
      message("Contingency table columns created and added to the data frame.")
    } else {
      stop("Contingency table columns are missing and target_corpus and reference_corpus are not specified.")
    }
  }

  # Adjust counts to avoid division by zero
  a_adj <- ifelse(tbl$a == 0, 0.5, tbl$a)
  b_adj <- ifelse(tbl$b == 0, 0.5, tbl$b)
  c_adj <- ifelse(tbl$c == 0, 0.5, tbl$c)
  d_adj <- ifelse(tbl$d == 0, 0.5, tbl$d)

  # Calculate log odds ratio
  log_odds <- log((a_adj * d_adj) / (b_adj * c_adj))

  # Add log odds to the data frame
  tbl_with_logOdds <- tbl %>%
    mutate(
      log_odds = log_odds
    )

  return(tbl_with_logOdds)
}
