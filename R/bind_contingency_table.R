#' Bind Contingency Table Columns
#'
#' Computes and binds the contingency table counts (`a`, `b`, `c`, `d`, `n1`, `n2`, `N`) to the data frame.
#' These counts are essential for calculating keyness metrics using other `bind_` functions.
#'
#' @param tbl A data frame containing token counts.
#' @param target_corpus The name of the column containing counts from the target corpus (as a string).
#' @param reference_corpus The name of the column containing counts from the reference corpus (as a string).
#'
#' @details This function calculates the necessary counts for a 2x2 contingency table for each token:
#' \describe{
#'   \item{\strong{a}}{Count of the token in the target corpus.}
#'   \item{\strong{b}}{Count of the token in the reference corpus.}
#'   \item{\strong{c}}{Total count of tokens in the target corpus minus \strong{a} (i.e., token absent in target corpus).}
#'   \item{\strong{d}}{Total count of tokens in the reference corpus minus \strong{b} (i.e., token absent in reference corpus).}
#'   \item{\strong{n1}}{Total count of tokens in the target corpus.}
#'   \item{\strong{n2}}{Total count of tokens in the reference corpus.}
#'   \item{\strong{N}}{Total count of tokens in both corpora (\strong{n1} + \strong{n2}).}
#' }
#'
#' The counts are converted to numeric to prevent integer overflow when dealing with large counts.
#'
#' @return A data frame with the original columns and additional contingency table columns:
#' \itemize{
#'   \item \code{a}, \code{b}, \code{c}, \code{d}: Counts for the contingency table.
#'   \item \code{n1}, \code{n2}: Total counts for the target and reference corpora, respectively.
#'   \item \code{N}: Total count of tokens in both corpora.
#' }
#'
#' @examples
#' library(dplyr)
#'
#' # Sample data
#' df <- data.frame(
#'   token = c("word1", "word2"),
#'   target_count = c(10, 5),
#'   reference_count = c(5, 10)
#' )
#'
#' # Compute contingency table
#' df_with_contingency <- bind_contingency_table(
#'   tbl = df,
#'   target_corpus = "target_count",
#'   reference_corpus = "reference_count"
#' )
#'
#' # View the result
#' print(df_with_contingency)
#'
#' @export
bind_contingency_table <- function(tbl, target_corpus, reference_corpus) {
  # Calculate total counts for target and reference corpora
  total_target <- as.numeric(sum(tbl[[target_corpus]], na.rm = TRUE))
  total_reference <- as.numeric(sum(tbl[[reference_corpus]], na.rm = TRUE))

  # Convert counts to numeric to prevent integer overflow
  tbl <- tbl %>%
    mutate(
      a = as.numeric(.data[[target_corpus]]),             # Token present in target corpus
      b = as.numeric(.data[[reference_corpus]]),          # Token present in reference corpus
      c = as.numeric(total_target - a),                   # Token absent in target corpus
      d = as.numeric(total_reference - b),                # Token absent in reference corpus
      n1 = as.numeric(total_target),
      n2 = as.numeric(total_reference),
      N = as.numeric(total_target + total_reference)
    )

  return(tbl)
}
