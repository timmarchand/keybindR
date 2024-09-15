#' Bind Kullback-Leibler Divergence (KLD)
#'
#' Computes the Kullback-Leibler Divergence (KLD) for each token and adds it as a new column.
#' KLD measures how one probability distribution (target corpus) diverges from another reference distribution (reference corpus).
#'
#' @param tbl A data frame containing token counts.
#' @param target_corpus The name of the column containing counts from the target corpus (as a string).
#' @param reference_corpus The name of the column containing counts from the reference corpus (as a string).
#' @param signed Logical; if \code{TRUE}, the KLD is signed based on the difference between the proportions in the target and reference corpora.
#' @param normalized Logical; if \code{TRUE}, the KLD is normalized to a [0, 1] interval.
#'
#' @details
#' The Kullback-Leibler Divergence (KLD) is calculated as:
#' \deqn{KLD = P_{\text{present}} \log_2\left(\frac{P_{\text{present}}}{Q_{\text{present}}}\right) + P_{\text{absent}} \log_2\left(\frac{P_{\text{absent}}}{Q_{\text{absent}}}\right)}
#' where:
#' \itemize{
#'   \item \eqn{P_{\text{present}}} is the proportion of the token in the target corpus.
#'   \item \eqn{P_{\text{absent}}} is the proportion of the token absent in the target corpus.
#'   \item \eqn{Q_{\text{present}}} is the proportion of the token in the reference corpus.
#'   \item \eqn{Q_{\text{absent}}} is the proportion of the token absent in the reference corpus.
#' }
#'
#' If \code{normalized = TRUE}, the KLD is normalized to the [0, 1] interval using the formula:
#' \deqn{KLD_{\text{normalized}} = \frac{KLD_{\text{raw}}}{1 + KLD_{\text{raw}}}}
#'
#' @return A data frame with the original columns and an additional `KLD` column containing the Kullback-Leibler Divergence.
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
#' # Compute KLD (signed and normalized)
#' df_with_KLD <- bind_KLD(
#'   tbl = df,
#'   target_corpus = "target_count",
#'   reference_corpus = "reference_count",
#'   signed = TRUE,
#'   normalized = TRUE
#' )
#'
#' # View the result
#' print(df_with_KLD)
#'
#' @export
