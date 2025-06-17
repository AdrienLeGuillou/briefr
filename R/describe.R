#' Get a briefing of a data frame
#'
#' @inheritParams brf_summary_df
#' @param groupings The columns to use as grouping variables as a character
#'   vector.
#' @param output_filename The name of the .html report (default: "briefing.html")
#' @param output_dir The folder where to put the report. (default: `NULL`)
#'   If set to `NULL` the report will be put in the current working directory
#'   Otherwise this parameter will be passed to the `output_dir` parameter of rmarkdown::render()
#'
#' @export
brf_describe <- function(df, groupings = NULL, na.rm = FALSE,
                         output_filename = "briefing.html",
                         output_dir = NULL, non_grouped = TRUE) {

  if (is.null(output_dir)) output_dir <- getwd()

  rmarkdown::render(
    system.file("rmd/describe.Rmd", package = "briefr"),
    output_file = output_filename,
    output_dir = output_dir,
    params = list(
      df = df,
      groupings = groupings,
      na.rm = na.rm,
      non_grouped = non_grouped
    )
  )
}
