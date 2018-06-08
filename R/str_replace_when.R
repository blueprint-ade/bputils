#' Replace substrings in a vector sequentially, with arguments structured
#' like case_when()
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#'
str_replace_when <- function(x, ...) {

  f <- list2(...)
  n <- length(f)

  if(n == 0) {
    abort("No cases provided")
  }

  p <- map_chr(f, ~.[[2]])
  r <- map_chr(f, ~.[[3]])

  for(i in seq_len(n)) {
    x <- str_replace(x, p[i], r[i])
  }

  x

}
