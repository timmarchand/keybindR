#' Remove Contingency Table Columns
#'
#' Removes the contingency table columns (`a`, `b`, `c`, `d`, `n1`, `n2`, `N`) from the data frame.
#' This function is useful for cleaning up the data frame after computing keyness metrics
#' when the contingency table columns are no longer needed.
#' It safely removes only the columns that exist in the data frame.
#'
#' @param tbl A data frame potentially containing the contingency table columns to be removed.
#'
#' @return A data frame with the contingency table columns removed if they were present.
#'
#' @examples
#' library(dplyr)
#'
#' # Sample data with some contingency table columns
#' df <- data.frame(
#'   token = c("word1", "word2"),
#'   a = c(10, 5),
#'   n1 = c(100, 100)
#' )
#'
#' # Remove contingency table columns
#' df_clean <- remove_contingency_table(df)
#'
#' # View the result
#' print(df_clean)
#'
#' @export
remove_contingency_table <- function(tbl) {
  contingency_cols <- c("a", "b", "c", "d", "n1", "n2", "N")
  existing_cols <- contingency_cols[contingency_cols %in% names(tbl)]
  tbl <- tbl %>%
    select(-all_of(existing_cols))
  return(tbl)
}
