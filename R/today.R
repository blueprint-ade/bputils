
#' Paste today's date into a string with magic
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
`%today%` <- function(x, y) {

  paste0(x, Sys.Date(), y)

}
