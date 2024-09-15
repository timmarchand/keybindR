#' Bind G-Test (G2) Statistic
#'
#' Computes the G-Test (G2) statistic for each token and adds it as a new column.
#' The G-Test is used to measure the association between a token and the target corpus compared to the reference corpus.
#' It compares the observed frequency of a token to the expected frequency, testing the null hypothesis of independence between the target and reference corpora.
#'
#' @param tbl A data frame containing token counts.
#' @param target_corpus The name of the column containing counts from the target corpus (as a string).
#' @param reference_corpus The name of the column containing counts from the reference corpus (as a string).
#' @param signed Logical; if \code{TRUE}, the G2 statistic is multiplied by the sign of the residuals to indicate the direction
#' of association between the target and reference corpora.
#'
#' @details
#' The G-Test (G2) statistic is calculated as:
#' \deqn{G2 = 2 \times \sum O \times \log\left(\frac{O}{E}\right)}
#' where:
#' \itemize{
#'   \item \eqn{O} is the observed count (from the contingency table).
#'   \item \eqn{E} is the expected count (calculated using the marginal totals of the contingency table).
#' }
#'
#' If the necessary contingency table columns (`a`, `b`, `c`, `d`) are not present in the data frame,
#' and `target_corpus` and `reference_corpus` are specified, the function will compute the contingency table.
#' If the columns are missing and the corpus columns are not specified, the function will stop with an error.
#'
#' If \code{signed = TRUE}, the G2 statistic is multiplied by the sign of the residuals to reflect the direction of the association,
#' indicating whether the token is more likely to occur in the target or reference corpus.
#'
#' @return A data frame with the original columns and an additional `G2` column containing the G-Test statistic.
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
#' # Compute G2 statistic (signed)
#' df_with_G2 <- bind_G2(
#'   tbl = df,
#'   target_corpus = "target_count",
#'   reference_corpus = "reference_count",
#'   signed = TRUE
#' )
#'
#' # View the result
#' print(df_with_G2)
#' @importFrom dplyr %>% mutate rowwise select ungroup
#' @importFrom stats chisq.test
#' @export
bind_G2 <- function(tbl, target_corpus = NULL, reference_corpus = NULL, signed = FALSE) {
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

  tbl_with_G2 <- tbl %>%
    rowwise() %>%
    mutate(
      observed = list(c(a, b, c, d)),  # Store observed counts as a list
      expected_matrix = list(chisq.test(matrix(unlist(observed), nrow = 2))$expected),  # Store expected as a list
      # Ensure observed and expected are numeric
      observed = list(as.numeric(unlist(observed))),
      expected = list(as.numeric(unlist(expected_matrix))),
      G2 = 2 * sum(observed * log(observed / expected), na.rm = TRUE)
    ) %>%
    ungroup()

  # Apply the 'signed' parameter
  if (signed) {
    tbl_with_G2 <- tbl_with_G2 %>%
      rowwise() %>%
      mutate(
        residuals = list(chisq.test(matrix(observed, nrow =2))$residuals),
        sign = sign(residuals[1, 1]),
        G2_signed = G2 * sign
      ) %>%
      ungroup() %>%
      mutate(G2 = G2_signed)
  }

  tbl_with_G2 <- tbl_with_G2 %>%
    select(-observed, -expected_matrix, -expected, -residuals, -sign, -G2_signed)

  return(tbl_with_G2)
}
