#' Bind Cohen's d Effect Size
#'
#' Calculates Cohen's d effect size for each token and adds it as a new column.
#' Cohen's d measures the standardized difference between two proportions and is useful
#' for comparing the relative importance of tokens between the target and reference corpora.
#'
#' @param tbl A data frame containing token counts.
#' @param target_corpus The name of the column containing counts from the target corpus (as a string).
#' @param reference_corpus The name of the column containing counts from the reference corpus (as a string).
#'
#' @details
#' The function computes Cohen's d using the proportions of each token in the target and reference corpora.
#' The effect size is calculated as:
#' \deqn{d = \frac{p_1 - p_2}{s_p}}
#' where:
#' \itemize{
#'   \item \eqn{p_1} is the proportion of the token in the target corpus.
#'   \item \eqn{p_2} is the proportion of the token in the reference corpus.
#'   \item \eqn{s_p} is the pooled standard deviation, calculated as:
#'     \deqn{s_p = \sqrt{\frac{(p_1 (1 - p_1) + p_2 (1 - p_2))}{2}}}
#' }
#' A small constant is added to \eqn{s_p} when it equals zero to prevent division by zero.
#'
#' If the necessary contingency table columns (`a`, `b`, `n1`, `n2`) are not present in the data frame,
#' and `target_corpus` and `reference_corpus` are specified, the function will compute the contingency table.
#' If the columns are missing and the corpus columns are not specified, the function will stop with an error.
#'
#' @return A data frame with the original columns and an additional `cohen_d` column containing the effect sizes.
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
#' # Compute Cohen's d
#' df_with_cohenD <- bind_cohenD(
#'   tbl = df,
#'   target_corpus = "target_count",
#'   reference_corpus = "reference_count"
#' )
#'
#' # View the result
#' print(df_with_cohenD)
#'
#' @export
bind_cohenD <- function(tbl, target_corpus = NULL, reference_corpus = NULL) {
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

  p1 <- tbl$a / tbl$n1
  p2 <- tbl$b / tbl$n2
  s_p <- sqrt( (p1 * (1 - p1) + p2 * (1 - p2)) / 2 )
  s_p <- ifelse(s_p == 0, 1e-10, s_p)  # Add small constant to prevent division by zero
  cohen_d <- (p1 - p2) / s_p

  tbl_with_cohenD <- tbl %>%
    mutate(
      cohen_d = cohen_d
    )

  return(tbl_with_cohenD)
}
