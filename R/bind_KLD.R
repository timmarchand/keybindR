#' Bind Kullback-Leibler Divergence (KLD) to Data Frame
#'
#' This function computes the Kullback-Leibler Divergence (KLD) for each token in the target corpus compared to the reference corpus and adds it as a new column to the data frame.
#'
#' @param tbl A data frame containing token counts for both the target and reference corpora.
#' @param target_corpus The name of the column containing counts from the target corpus (as a string).
#' @param reference_corpus The name of the column containing counts from the reference corpus (as a string).
#' @param signed Logical; if \code{TRUE}, the KLD is signed based on the difference in proportions between the target and reference corpora.
#' @param normalized Logical; if \code{TRUE}, the KLD is normalized to a [0,  1] interval using the formula \eqn{KLD_{normalized} = \frac{KLD_{raw}}{1 + KLD_{raw}}}.
#'
#' @details
#' The Kullback-Leibler Divergence (KLD) is calculated as:
#' \deqn{KLD = P_{\text{present}} \log_2\left(\frac{P_{\text{present}}}{Q_{\text{present}}}\right) + P_{\text{absent}} \log_2\left(\frac{P_{\text{absent}}}{Q_{\text{absent}}}\right)}
#' where:
#' \itemize{
#'   \item \eqn{P_{\text{present}}} is the proportion of the token present in the target corpus.
#'   \item \eqn{P_{\text{absent}}} is the proportion of the token absent in the target corpus.
#'   \item \eqn{Q_{\text{present}}} is the proportion of the token present in the reference corpus.
#'   \item \eqn{Q_{\text{absent}}} is the proportion of the token absent in the reference corpus.
#' }
#'
#' If \code{signed = TRUE}, the KLD is multiplied by -1 when the token is proportionally less frequent in the target corpus than in the reference corpus. If \code{normalized = TRUE}, the KLD is normalized to a [0,  1] interval.
#'
#' @return A data frame with the original columns and an additional `KLD` column containing the computed Kullback-Leibler Divergence for each token.
#'
#' @examples
#' library(dplyr)
#'
#' # Example data
#' df <- data.frame(
#'   token = c("word1", "word2", "word3"),
#'   target_count = c(30, 50, 20),
#'   reference_count = c(40, 30, 10)
#' )
#'
#' # Compute KLD with signed and normalized options
#' df_with_KLD <- bind_KLD(
#'   tbl = df,
#'   target_corpus = "target_count",
#'   reference_corpus = "reference_count",
#'   signed = TRUE,
#'   normalized = TRUE
#' )
#'
#' print(df_with_KLD)
#'
#' @export
bind_KLD <- function(tbl, target_corpus = NULL, reference_corpus = NULL, signed = TRUE, normalized = TRUE) {
  # Check if contingency table columns are present
  required_cols <- c("a", "b", "c", "d", "n1", "n2", "N")
  if (!all(required_cols %in% names(tbl))) {
    if (!is.null(target_corpus) && !is.null(reference_corpus)) {
      tbl <- bind_contingency_table(tbl, target_corpus, reference_corpus)
      message("Contingency table columns created and added to the data frame.")
    } else {
      stop("Contingency table columns are missing and target_corpus and reference_corpus are not specified.")
    }
  }

  # Proceed with the calculations
  tbl_with_KLD <- tbl %>%
    mutate(
      P_present = a / n1,
      P_absent = c / n1,
      Q_present = b / n2,
      Q_absent = d / n2,
      # Replace zeros with small values to avoid log(0)
      P_present = ifelse(P_present == 0, 1e-10, P_present),
      P_absent  = ifelse(P_absent == 0, 1e-10, P_absent),
      Q_present = ifelse(Q_present == 0, 1e-10, Q_present),
      Q_absent  = ifelse(Q_absent == 0, 1e-10, Q_absent),
      KLD_raw = P_present * log2(P_present / Q_present) + P_absent * log2(P_absent / Q_absent)
    )

  # Apply normalization
  if (normalized) {
    tbl_with_KLD <- tbl_with_KLD %>%
      mutate(
        KLD_normalized = KLD_raw / (1 + KLD_raw)
      )
  } else {
    tbl_with_KLD <- tbl_with_KLD %>%
      mutate(
        KLD_normalized = KLD_raw
      )
  }

  # Apply the 'signed' parameter
  if (signed) {
    tbl_with_KLD <- tbl_with_KLD %>%
      mutate(
        proportion_diff = P_present - Q_present,
        KLD_signed = ifelse(proportion_diff < 0, -KLD_normalized, KLD_normalized)
      )
  } else {
    tbl_with_KLD <- tbl_with_KLD %>%
      mutate(
        KLD_signed = KLD_normalized
      )
  }

  tbl_with_KLD <- tbl_with_KLD %>%
    mutate(KLD = KLD_signed) %>%
    select(-P_present, -P_absent, -Q_present, -Q_absent, -KLD_raw, -KLD_normalized, -KLD_signed, -proportion_diff)

  return(tbl_with_KLD)
}
