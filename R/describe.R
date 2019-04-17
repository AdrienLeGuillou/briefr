#' Get a briefing of a data frame
#'
#' @export
brf_describe <- function(df, groupings = NULL, na.rm = F,
                         output_filename = "briefing.html",
                         output_dir = NULL) {

  if (is.null(output_dir)) output_dir <- getwd()

  rmarkdown::render(
    system.file("rmd/describe.Rmd", package = "briefr"),
    output_file = output_filename,
    output_dir = output_dir,
    params = list(df = df, groupings = groupings, na.rm = na.rm)
  )
}
