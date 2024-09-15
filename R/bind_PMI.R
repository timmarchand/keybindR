#' Bind Pointwise Mutual Information (PMI)
#'
#' Computes the Pointwise Mutual Information (PMI) for each token and adds it as a new column.
#' PMI measures the association between a token and the target corpus compared to the reference corpus.
#'
#' @param tbl A data frame containing token counts.
#' @param target_corpus The name of the column containing counts from the target corpus (as a string).
#' @param reference_corpus The name of the column containing counts from the reference corpus (as a string).
#' @param discount A small value to add to counts to handle zero counts (default is 0).
#'
#' @details
#' PMI is calculated as:
#' \deqn{PMI = \log_2\left(\frac{a \times N}{n_1 \times y_w}\right)}
#' where:
#' \itemize{
#'   \item \eqn{a} is the count of the token in the target corpus.
#'   \item \eqn{N} is the total number of tokens in both corpora.
#'   \item \eqn{n_1} is the total number of tokens in the target corpus.
#'   \item \eqn{y_w} is the total count of the token across both corpora.
#' }
#' The \code{discount} parameter is used to handle zero counts by adding a small value to the counts.
#' If the necessary contingency table columns (`a`, `b`, `n1`, `N`) are not present in the data frame,
#' and `target_corpus` and `reference_corpus` are specified, the function will compute the contingency table.
#' If the columns are missing and the corpus columns are not specified, the function will stop with an error.
#'
#' @return A data frame with the original columns and an additional `PMI` column containing the PMI values.
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
#' # Compute PMI with a discount for zero counts
#' df_with_PMI <- bind_PMI(
#'   tbl = df,
#'   target_corpus = "target_count",
#'   reference_corpus = "reference_count",
#'   discount = 1
#' )
#'
#' # View the result
#' print(df_with_PMI)
#'
#' @export
bind_PMI <- function(tbl, target_corpus = NULL, reference_corpus = NULL, discount = 0) {
  # Check if contingency table columns are present
  required_cols <- c("a", "b", "n1", "N")
  if (!all(required_cols %in% names(tbl))) {
    if (!is.null(target_corpus) && !is.null(reference_corpus)) {
      tbl <- bind_contingency_table(tbl, target_corpus, reference_corpus)
      message("Contingency table columns created and added to the data frame.")
    } else {
      stop("Contingency table columns are missing and target_corpus and reference_corpus are not specified.")
    }
  }

  # Adjust counts with discount
  a_adj <- tbl$a + discount
  n1_adj <- tbl$n1 + discount * nrow(tbl)
  y_w_adj <- tbl$a + tbl$b + discount * 2

  # Compute PMI
  PMI <- log2((a_adj * tbl$N) / (n1_adj * y_w_adj))

  # Add the PMI column to the data frame
  tbl_with_PMI <- tbl %>%
    mutate(
      PMI = PMI
    )

  return(tbl_with_PMI)
}
