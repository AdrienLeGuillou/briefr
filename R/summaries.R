brf_summary_num <- function(df, data_col, grouping_col = NULL) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  df <- dplyr::ungroup(df)

  if (!rlang::quo_is_null(grouping_col)) {
    df <- dplyr::group_by(df, !!grouping_col)
  }

  date_summary <- is(dplyr::pull(df, !!data_col), "Date")

  out <-
    df %>%
    dplyr::summarise(
      n = dplyr::n(),
      missing = sum(is.na(!!data_col)),
      prop_missing = mean(is.na(!!data_col)),
      p0 = quantile(as.numeric(!!data_col), probs = 0, na.rm = T),
      p25 = quantile(as.numeric(!!data_col), probs = 0.25, na.rm = T),
      p50 = quantile(as.numeric(!!data_col), probs = 0.5, na.rm = T),
      p75 = quantile(as.numeric(!!data_col), probs = 0.75, na.rm = T),
      p100 = quantile(as.numeric(!!data_col), probs = 1, na.rm = T),
      mean = mean(!!data_col, na.rm = T),
      sd = sd(!!data_col, na.rm = T),
      skew = moments::skewness(!!data_col, na.rm = T),
      kurt = moments::kurtosis(!!data_col, na.rm = T)
    )

  if (date_summary) {
    out <- dplyr::mutate_at(
      out,
      dplyr::vars(dplyr::matches("^p[0-9]+")),
      function(x) as.Date(x, origin = "1970-01-01")
    )
  }

  out
}

brf_summary_cat <- function(df, data_col, grouping_col = NULL) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  df <- dplyr::ungroup(df) %>%
    dplyr::mutate(!!data_col := factor_infreq(as.factor(!!data_col)))

  if (!rlang::quo_is_null(grouping_col)) {
    df <- dplyr::group_by(df, !!grouping_col)
  }

  df %>%
    dplyr::summarise(
      n = dplyr::n(),
      levels = length(levels(!!data_col)),
      mode = levels(!!data_col)[1],
      prop_mode = mean(!!data_col == levels(!!data_col)[1], na.rm = T),
      missing = sum(is.na(!!data_col)),
      prop_missing = mean(is.na(!!data_col))
    )
}

brf_summary_cat_lvl <- function(df, data_col, grouping_col = NULL,
                                       na.rm = F) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  df <- dplyr::ungroup(df)
  groupers <- c(data_col)

  if (!rlang::quo_is_null(grouping_col)) {
    groupers <- c(grouping_col, groupers)
  }

  if (na.rm) df <- na.omit(dplyr::select(df, !!!groupers))

  df %>%
    dplyr::group_by(!!!groupers) %>%
    dplyr::summarise(
      n = dplyr::n()
    ) %>%
    dplyr::mutate(
      prop = n / sum(n)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prop_overall = n / sum(n)) %>%
    dplyr::select(!!!groupers, n, prop, prop_overall)
}
