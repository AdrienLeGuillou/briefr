#' Format a data frame summary with `kable()`
#'
#' @param df the data frame to summarise
#' @return the summary as a `kable table`
#'
#' @export
brf_formatted_df <- function(df) {
  df <- brf_summary_df(df)

  tab <- kableExtra::kable(df, align = rep("c", 4))

  dflt_kable(df)
}

#' Format a numerical column summary with `kable()`
#'
#' @inheritParams brf_summary_num
#' @inherit brf_formatted_df return
#'
#' @export
brf_formatted_num <- function(df, data_col, grouping_col = NULL, decimals = 1) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  # convert `decimals` to the `accuracy` equivalent for `scales`
  accuracy <- 10^(-decimals)

  df <- dplyr::ungroup(df)
  date_gt <- is(dplyr::pull(df, !!data_col), "Date")

  df <- brf_summary_num(df, !!data_col, !!grouping_col)

  # format percentiles and mean, with special format for dates
  if (date_gt) {
    df <- dplyr::mutate_at(
      df,
      dplyr::vars(dplyr::matches("(p[0-9])|(mean)")),
      scales::date_format()
    )
  } else {
    df <- dplyr::mutate_at(
      df,
      dplyr::vars(dplyr::matches("(p[0-9])|(mean)")),
      function(x) scales::number(x, accuracy = accuracy)
    )
  }

  # format the other columns
  df <- df %>%
    dplyr::mutate_at(
      dplyr::vars(n, missing),
      scales::number_format(accuracy = 1)
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(sd, skew, kurt),
      scales::number_format(accuracy = accuracy)
    ) %>%
    dplyr::mutate(
      prop_missing = scales::percent(prop_missing, accuracy = accuracy),
      missing = paste0(missing, " (", prop_missing, ")")
    ) %>%
    dplyr::select(-prop_missing)

  dflt_kable(df, !!grouping_col)
}

#' Format a categorical column summary with `kable()`
#'
#' @inheritParams brf_summary_cat
#' @inherit brf_formatted_df return
#'
#' @export
brf_formatted_cat <- function(df, data_col, grouping_col = NULL, decimals = 1) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  # convert `decimals` to the `accuracy` equivalent for `scales`
  accuracy <- 10^(-decimals)

  df <- dplyr::ungroup(df)
  df <- brf_summary_cat(df, !!data_col, !!grouping_col)

  df <- df %>%
    dplyr::mutate_at(
      dplyr::vars(n, levels, missing),
      scales::number_format(accuracy = 1)
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(prop_mode, prop_missing),
      scales::percent_format(accuracy = accuracy)
    ) %>%
    dplyr::mutate(
      mode = paste0(mode, " (", prop_mode, ")"),
      missing = paste0(missing, " (", prop_missing, ")")
    ) %>%
    dplyr::select(-c(prop_mode, prop_missing))

  dflt_kable(df, !!grouping_col)
}

#' Format a categorical column levels summary with `gt()`
#'
#' @inheritParams brf_summary_cat_lvl
#' @inherit brf_formatted_df return
#'
#' @export
brf_formatted_cat_lvl <- function(df, data_col, grouping_col = NULL,
                                  na.rm = F, decimals = 1) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  # convert `decimals` to the `accuracy` equivalent for `scales`
  accuracy <- 10^(-decimals)

  has_groups <- !rlang::quo_is_null(grouping_col)

  df <- dplyr::ungroup(df)
  df <- brf_summary_cat_lvl(df, !!data_col, !!grouping_col, na.rm = na.rm) %>%
    tidyr::complete(
      !!grouping_col, !!data_col,
      fill = list(n = 0, prop = 0, prop_overall = 0)
    )

  df <- df %>%
    dplyr::mutate(
      n = scales::number(n, accuracy = 1),
      prop = scales::percent(prop, accuracy = accuracy),
      n = paste0(n, " (", prop, ")")
    ) %>%
    dplyr::select(-c(prop, prop_overall))


  df <- df %>%
    tidyr::pivot_wider(
      names_from = !!data_col, values_from = n,
      names_prefix = paste0(rlang::as_label(data_col), ": ")
    )

  dflt_kable(df, !!grouping_col)
}

