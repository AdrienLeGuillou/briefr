#' Summarise a data frame
#'
#' Produce the summary of a data frame.
#'
#' @param df the data frame to summarise
#' @return a one-row data with summarising df
#'
#' @export
brf_summary_df <- function(df) {
  out <- data.frame(
    n_rows = nrow(df),
    n_complete = sum(complete.cases(df)),
    prop_complete = mean(complete.cases(df)),
    n_cols = ncol(df)
  )

  out
}

#' Summarise a numerical column in a data frame with a grouping option
#'
#' @param df a data frame
#' @param data_col the unquoted name of the column to summarise
#' @param grouping_col the unquoted name of the column to use for groupings
#' @return a dataframe containing the summary informations
#'
#' @export
brf_summary_num <- function(df, data_col, grouping_col = NULL) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  df <- dplyr::ungroup(df)

  if (!rlang::quo_is_null(grouping_col)) {
    df <- dplyr::group_by(df, !!grouping_col)
  }

  date_summary <- is(dplyr::pull(df, !!data_col), "Date")

  out <-
    df |>
    dplyr::summarise(
      n = dplyr::n(),
      missing = sum(is.na(!!data_col)),
      prop_missing = mean(is.na(!!data_col)),
      p0 = quantile(as.numeric(!!data_col), probs = 0, na.rm = TRUE),
      p25 = quantile(as.numeric(!!data_col), probs = 0.25, na.rm = TRUE),
      p50 = quantile(as.numeric(!!data_col), probs = 0.5, na.rm = TRUE),
      p75 = quantile(as.numeric(!!data_col), probs = 0.75, na.rm = TRUE),
      p100 = quantile(as.numeric(!!data_col), probs = 1, na.rm = TRUE),
      mean = mean(!!data_col, na.rm = TRUE),
      sd = sd(!!data_col, na.rm = TRUE),
      skew = moments::skewness(!!data_col, na.rm = TRUE),
      kurt = moments::kurtosis(!!data_col, na.rm = TRUE)
    )

  if (date_summary) {
    out <- dplyr::mutate(dplyr::across(
      dplyr::matches("^p[0-9]+"),
      function(x) as.Date(x, origin = "1970-01-01")
    ))
  }

  out
}

#' Summarise a categorical column in a data frame with a grouping option
#'
#' @inheritParams brf_summary_num
#' @inherit brf_summary_num return
#' @export
brf_summary_cat <- function(df, data_col, grouping_col = NULL) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  df <- dplyr::ungroup(df) |>
    dplyr::mutate(!!data_col := factor_infreq(as.factor(!!data_col)))

  if (!rlang::quo_is_null(grouping_col)) {
    df <- dplyr::group_by(df, !!grouping_col)
  }

  df |>
    dplyr::summarise(
      n = dplyr::n(),
      levels = length(levels(!!data_col)),
      mode = levels(!!data_col)[1],
      prop_mode = mean(!!data_col == levels(!!data_col)[1], na.rm = TRUE),
      missing = sum(is.na(!!data_col)),
      prop_missing = mean(is.na(!!data_col))
    )
}

#' Summarise the levels of a categorical column in a data frame with a grouping option
#'
#' @inheritParams brf_summary_num
#' @param na.rm should `NA`s be removed (default: FALSE)
#' @inherit brf_summary_num return
#' @export
brf_summary_cat_lvl <- function(df, data_col, grouping_col = NULL,
                                na.rm = FALSE) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  df <- dplyr::ungroup(df)
  groupers <- c(data_col)

  if (!rlang::quo_is_null(grouping_col)) {
    groupers <- c(grouping_col, groupers)
  }

  if (na.rm) df <- na.omit(dplyr::select(df, !!!groupers))

  df |>
    dplyr::group_by(!!!groupers) |>
    dplyr::summarise(
      n = dplyr::n()
    ) |>
    dplyr::mutate(
      prop = n / sum(n)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(prop_overall = n / sum(n)) |>
    dplyr::select(!!!groupers, n, prop, prop_overall)
}
