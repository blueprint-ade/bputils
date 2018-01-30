#' Magically grepl without NAs
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
`%find%` <- function(x, y) {

  y <- paste0(y, collapse = "|")

  !is.na(x) & grepl(y, x)

}


#' Find observations in a vector that are not NA and don't match a value
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
`%!find%` <- function(x, y) {

  y <- paste0(y, collapse = "|")

  !is.na(x) & !grepl(y, x)

}
