# Suppress global variable warnings for column names used in dplyr functions
utils::globalVariables(c(
  "a", "all_of", "b", "c", "d", "n1", "n2", "observed", "expected",
  "expected_matrix", "G2", "G2_signed", "P_present", "P_absent",
  "Q_present", "Q_absent", "KLD_raw", "KLD_normalized", "KLD_signed",
  "proportion_diff", "residuals", ".data"
))
