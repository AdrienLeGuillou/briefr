#' Make a histogram of a numerical column in a data frame
#'
#' @inheritParams brf_summary_num
#' @param na.rm should `NA`s be removed (default: FALSE)
#' @return a ggplot object
#'
#' @export
brf_plot_num_hist <- function(df, num_col, grouping_col = NULL, na.rm = FALSE) {
  num_col <- rlang::enquo(num_col)
  grouping_col <- rlang::enquo(grouping_col)

  bins <- sum( !is.na(dplyr::pull(df, !!num_col)) )

  default_layer <- list(ggplot2::theme_minimal())

  aesthetic <- ggplot2::aes(x = !!num_col)
  selection <- c(num_col)

  if (!rlang::quo_is_null(grouping_col)) {
    default_layer <- c(
      default_layer,
      ggplot2::facet_wrap(dplyr::vars(!!grouping_col), nrow = 1)
    )

    bins <- bins / length(unique(df[[rlang::as_name(grouping_col)]]))

    aesthetic <- c(aesthetic, ggplot2::aes(fill = !!grouping_col))
    class(aesthetic) <- "uneval"

    selection <- c(selection, grouping_col)
  }

  if (na.rm) df <- na.omit(dplyr::select(df, !!!selection))

  p <-
    ggplot2::ggplot(df, aesthetic) +
    ggplot2::geom_histogram(bins = min(round(sqrt(bins), 0), 60)) +
    default_layer

  if (rlang::quo_is_null(grouping_col)) {
    p$layers[[1]]$aes_params$fill <- "#337ab7"
  }

  p
}

#' Make a violin plot of a numerical column in a data frame
#'
#' @inheritParams brf_summary_num
#' @param na.rm should `NA`s be removed (default: FALSE)
#' @return a ggplot object
#'
#' @export
brf_plot_num_violin <- function(df, num_col, grouping_col = NULL, na.rm = FALSE) {
  num_col <- rlang::enquo(num_col)
  grouping_col <- rlang::enquo(grouping_col)

  default_layer <- list(
    ggplot2::theme_minimal(),
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  )
  aesthetic <- ggplot2::aes(y = !!num_col)
  selection <- c(num_col)

  if (!rlang::quo_is_null(grouping_col)) {
    default_layer <- c(
      default_layer,
      ggplot2::facet_wrap(dplyr::vars(!!grouping_col), nrow = 1)
    )
    aesthetic <- c(
      aesthetic,
      ggplot2::aes(fill = !!grouping_col))
    class(aesthetic) <- "uneval"

    selection <- c(selection, grouping_col)
  }
  aesthetic <- c(aesthetic, ggplot2::aes(x = "all"))
  class(aesthetic) <- "uneval"

  default_layer <- c(
    default_layer,
    list(
      ggplot2::xlab(ifelse(!rlang::quo_is_null(grouping_col),
                  rlang::as_name(grouping_col),
                  "")),
      ggplot2::theme(axis.text.x = ggplot2::element_blank())
    )
  )


  if (na.rm) df <- na.omit(dplyr::select(df, !!!selection))

  # default_layer
  p <-
    ggplot2::ggplot(df, aesthetic) +
    ggplot2::geom_violin(alpha = 1) +
    ggplot2::geom_boxplot(width = 0.1, fill = "white", alpha = 0.8) +
    default_layer

  if (rlang::quo_is_null(grouping_col))
    p$layers[[1]]$aes_params$fill <- "#337ab7"

  p
}

#' Make a plot of a categorical column in a data frame, count barplot
#'
#' @inheritParams brf_summary_cat_lvl
#' @return a ggplot object
#'
#' @export
brf_plot_cat_count <- function(df, data_col, grouping_col = NULL, na.rm = FALSE) {
  data_col <- rlang::enquo(data_col)
  grouping_col <- rlang::enquo(grouping_col)

  default_layer <- list(
    ggplot2::theme_minimal(),
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  )

  aesthetic <- ggplot2::aes(fill = !!data_col)
  selection <- c(data_col)

  if (!rlang::quo_is_null(grouping_col)) {

    aesthetic <- c(aesthetic, ggplot2::aes(x = !!grouping_col))
    class(aesthetic) <- "uneval"

    selection <- c(selection, grouping_col)
  } else {
    aesthetic <- c(aesthetic, ggplot2::aes(x = "all"))
    class(aesthetic) <- "uneval"

    default_layer <- c(
      default_layer,
      list(
        ggplot2::xlab(""),
        ggplot2::theme(axis.text.x = ggplot2::element_blank())
      )
    )
  }

  if (na.rm) df <- na.omit(dplyr::select(df, !!!selection))

  p <-
    ggplot2::ggplot(df, aesthetic) +
    ggplot2::geom_bar(position = "dodge") +
    ggplot2::geom_text(
      ggplot2::aes(
        y = ..count../2,
        label = scales::percent(
          ..count../tapply(..count.., ..x.., sum, na.rm = T)[..x..],
          accuracy = 1
        )
      ),
      stat = "count",
      position = ggplot2::position_dodge(.9),
      size = 3
    ) +
    default_layer

  p
}
