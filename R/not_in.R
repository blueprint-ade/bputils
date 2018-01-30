
#' Inverse %in% operator
#'
#' @param x the thing
#' @param y what it is / isn't in
#'
#' @return logical vector, true when x is not in y
#' @export
#'
#' @examples
`%!in%` <- function(x, y) {

  !(x %in% y)

}
