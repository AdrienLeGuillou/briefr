#' Outputs an RMarkDown formatted chunk for a data frame summary
#'
#' @param df the data frame to summarise
brf_rmd_df <- function(df) {
  brf_formatted_df(df) %>%
    cat()

  cat("\n\n")
}

#' Outputs an RMarkDown formatted chunk for a numerical column
#'
#' @inheritParams brf_summary_num
#' @param na.rm should `NA`s be removed (default: FALSE)
brf_rmd_num <- function(df, data_col, grouping_col = NULL, na.rm = F) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  gridExtra::grid.arrange(
    brf_plot_num_hist(df, !!data_col, !!grouping_col) +
      ggplot2::theme(legend.position = "none"),
    brf_plot_num_violin(df, !!data_col, !!grouping_col) +
      ggplot2::theme(legend.position = "none"),
    ncol = 1
  )
  cat("\n\n")

  brf_formatted_num(df, !!data_col, !!grouping_col) %>%
    cat()

  cat("\n\n")
}

#' Outputs an RMarkDown formatted chunk for a categorical column
#'
#' @inheritParams brf_summary_cat_lvl
brf_rmd_cat <- function(df, data_col, grouping_col = NULL, na.rm = F) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  print(brf_plot_cat_count(df, !!data_col, !!grouping_col, na.rm = na.rm))
  cat("\n\n")

  brf_formatted_cat_lvl(df, !!data_col, !!grouping_col, na.rm = na.rm) %>%
    cat()
  cat("\n\n")
}

#' Choose the correct RMarkDown function depending on the column type
#'
#' @inheritParams brf_summary_cat_lvl
brf_rmd_dispatch <- function(df, data_col, grouping_col = NULL, na.rm = F) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  data_col_str <- rlang::as_label(data_col)

  if (!rlang::quo_is_null(grouping_col)) {
   grouping_col_str <- rlang::as_label(grouping_col)
  }

  cat(paste0(
    "## **`", data_col_str, "`**",
    ifelse(
      !rlang::quo_is_null(grouping_col),
      paste0(" - by *`", grouping_col_str, "`* "),
      ""
    ),"\n\n")
  )

  if (all(is.na( dplyr::pull(df, !!data_col) )))
    appropriate_rmd <- function(...) cat("Only NA's (missing values) \n\n")
  else if (any(class(dplyr::pull(df, !!data_col)) %in% c("numeric", "Date")))
    appropriate_rmd <- brf_rmd_num
  else if (any(
    class(dplyr::pull(df, !!data_col)) %in% c("character", "factor", "logical")))
    appropriate_rmd <- brf_rmd_cat
  else
    appropriate_rmd <- function(...) cat("No description available \n\n")

  tryCatch(
    appropriate_rmd(df, !!data_col, !!grouping_col, na.rm = na.rm),
    error = function(e) {
      cat("No description available \n\n")
      cat("The following error occured \n\n")
      cat(e)
      cat("\n\n")
    })
}
