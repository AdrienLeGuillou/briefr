brf_formatted_df <- function(df) {
  desc <- brf_summary_df(df)

  tab1 <-
    desc %>%
    gt::gt()

  tab1
}

brf_formatted_num <- function(df, data_col, grouping_col = NULL, decimals = 1) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  df <- dplyr::ungroup(df)
  date_gt <- is(dplyr::pull(df, !!data_col), "Date")

  df <- brf_summary_num(df, !!data_col, !!grouping_col)

  if (!rlang::quo_is_null(grouping_col)) {
    df <- dplyr::arrange(df, !!grouping_col)
  }

  tab1 <-
    gt::gt(
      df,
      rowname_col = rlang::as_label(grouping_col)
    )

  if (date_gt) {
    tab1 <- tab1 %>%
      gt::fmt_date(
        columns = dplyr::matches("(p[0-9])|(mean)"),
        date_style = 13
      )
  } else {
    tab1 <- tab1 %>%
      gt::fmt_number(
        columns = dplyr::matches("(p[0-9])|(mean)"),
        decimals = decimals
      )
  }

  tab1 <- tab1 %>%
    gt::fmt_number(
      columns = dplyr::vars(n, missing),
      decimals = 0
    ) %>%
    gt::fmt_number(
      columns = dplyr::vars(sd, skew, kurt),
      decimals = decimals
    ) %>%
    gt::fmt_percent(
      columns = dplyr::starts_with("prop"),
      decimals = 1
    ) %>%
    gt::cols_merge(
      col_1 = dplyr::vars(missing),
      col_2 = dplyr::vars(prop_missing),
      pattern = "{1} ({2})"
    ) %>%
    gt::cols_align(align = "center", columns = dplyr::everything()) %>%
    gt::tab_header(
      gt::md(paste0("Summary of variable **`", rlang::as_label(data_col), "`**")),
      gt::md(ifelse(
        rlang::quo_is_null(grouping_col), "",
        paste0("Grouped by *`", rlang::as_label(grouping_col), "`*")
      ))
    )

  if (!rlang::quo_is_null(grouping_col)) {
    tab1 <- tab1 %>%
      gt::tab_style(
        style = gt::cells_styles(text_align = "right"),
        locations = gt::cells_stub()
      )
  }


  tab1
}

brf_formatted_cat <- function(df, data_col, grouping_col = NULL) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  df <- dplyr::ungroup(df)

  df <- brf_summary_cat(df, !!data_col, !!grouping_col)

  if (!rlang::quo_is_null(grouping_col)) {
    df <- dplyr::arrange(df, !!grouping_col)
  }

  tab1 <-
    gt::gt(
      df,
      rowname_col = rlang::as_label(grouping_col)
    ) %>%
    gt::fmt_percent(
      columns = dplyr::starts_with("prop"),
      decimals = 1
    ) %>%
    gt::cols_merge(
      col_1 = dplyr::vars(mode),
      col_2 = dplyr::vars(prop_mode),
      pattern = "{1} ({2})"
    ) %>%
    gt::cols_merge(
      col_1 = dplyr::vars(missing),
      col_2 = dplyr::vars(prop_missing),
      pattern = "{1} ({2})"
    ) %>%
    gt::cols_align(align = "center", columns = dplyr::everything()) %>%
    gt::tab_header(
      gt::md(paste0("Summary of variable **`", rlang::as_label(data_col), "`**")),
      gt::md(ifelse(
        rlang::quo_is_null(grouping_col), "",
        paste0("Grouped by *`", rlang::as_label(grouping_col), "`*")
      ))
    )

  if (!rlang::quo_is_null(grouping_col)) {
    tab1 <- tab1 %>%
      gt::tab_style(
        style = gt::cells_styles(text_align = "right"),
        locations = gt::cells_stub()
      )
  }

  tab1
}

brf_formatted_cat_lvl <- function(df, data_col, grouping_col = NULL, na.rm = F) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  df <- dplyr::ungroup(df)

  df <- brf_summary_cat_lvl(df, !!data_col, !!grouping_col, na.rm = na.rm)

  if (!rlang::quo_is_null(grouping_col)) {
    df <- df %>%
      dplyr::arrange(!!data_col, !!grouping_col) %>%
      dplyr::mutate(!!grouping_col := tidyr::replace_na(!!grouping_col, "NA"))
  } else {
    df <- dplyr::arrange(df, !!data_col)
  }

  gt::gt(
    df,
    groupname_col = rlang::as_label(grouping_col),
    rowname_col = rlang::as_label(data_col)
  ) %>%
    gt::summary_rows(
      groups = TRUE,
      columns = dplyr::vars(prop_overall),
      fns = list(total = ~sum(.)),
      formatter = gt::fmt_percent, decimals = 1
    ) %>%
    gt::summary_rows(
      groups = TRUE,
      columns = dplyr::vars(n),
      fns = list(total = ~sum(.)),
      decimals = 0
    ) %>%
    gt::fmt_percent(
      columns = dplyr::starts_with("prop"),
      decimals = 1
    ) %>%
    gt::cols_align(align = "center", columns = dplyr::everything()) %>%
    gt::tab_options(
      row_group.font.weight = "bold"
    ) %>%
    gt::tab_style(
      style = gt::cells_styles(text_align = "right"),
      locations = gt::cells_stub()
    ) %>%
    gt::tab_header(
      gt::md(paste0("Levels of variable **`", rlang::as_label(data_col), "`**")),
      gt::md(
        ifelse(
          rlang::quo_is_null(grouping_col), "",
          paste0("Grouped by *`", rlang::as_label(grouping_col), "`*")
        )
      )
    )
}

brf_formatted_cat_lvl_wide <- function(df, data_col, grouping_col, na.rm = F) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  df <- dplyr::ungroup(df)

  df <- brf_summary_cat_lvl(df, !!data_col, !!grouping_col, na.rm = na.rm)

  df <- df %>%
    dplyr::arrange(!!data_col, !!grouping_col) %>%
    dplyr::mutate(!!grouping_col := tidyr::replace_na(!!grouping_col, "NA"))


  df <-
    df %>%
    dplyr::select(-prop_overall) %>%
    tidyr::complete(!!data_col, !!grouping_col) %>%
    tidyr::nest(-c(!!data_col, !!grouping_col)) %>%
    tidyr::spread(!!grouping_col, data) %>%
    tidyr::unnest(.sep = "__")

  tab1 <-
    df %>%
    gt::gt(rowname_col = rlang::as_label(data_col)) %>%
    gt::tab_stubhead_label(rlang::as_label(data_col))

  tab1 %>%
    gt::cols_split_delim("__") %>%
    gt::fmt_percent(
      columns = dplyr::matches("__prop"),
      decimals = 1
    ) %>%
    gt::cols_align(align = "center", columns =  dplyr::everything()) %>%
    gt::tab_style(
      style = gt::cells_styles(text_align = "right"),
      locations = gt::cells_stub()
    ) %>%
    gt::tab_header(
      gt::md(paste0("Levels of variable **`", rlang::as_label(data_col), "`**")),
      gt::md(
        ifelse(
          rlang::quo_is_null(grouping_col), "",
          paste0("Grouped by *`", rlang::as_label(grouping_col), "`*")
        )
      )
    )
}
