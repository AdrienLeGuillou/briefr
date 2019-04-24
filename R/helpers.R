#' Reassign levels in a factor using frequency
#'
#' @param x a factor
#'
#' @return a factor ordered by decreasing frequencies of its levels
factor_infreq <- function(x) {
  reorder(x, x, FUN = function(x) -length(x))
}
