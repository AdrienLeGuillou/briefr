#' Reassign levels in a factor using frequency
#'
#' @param x a factor
#' @return a factor ordered by decreasing frequencies of its levels
factor_infreq <- function(x) {
  reorder(x, x, FUN = function(x) -length(x))
}


#' Defautl table format for my summaries
#'
#' @param df the data frame to make into a table
#' @param grouping_col the grouping variable if any
#'
#' @return a formatted kable table
dflt_kable <- function(df, grouping_col = NULL) {
  grouping_col <- rlang::enquo(grouping_col)

  if (!rlang::quo_is_null(grouping_col)) {
    df <- dplyr::arrange(df, !!grouping_col)
    tab <- kableExtra::kable(df, align = c("l", rep("c", ncol(df) - 1))) %>%
      kableExtra::column_spec(1, bold = T, border_right = T,
                              background = "white")
  } else {
    tab <- kableExtra::kable(df, align = rep("c", ncol(df)))
  }

  kableExtra::kable_styling(
    tab,
    bootstrap_option = c("striped")
  )
}