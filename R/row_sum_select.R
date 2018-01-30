

#' Sum selected rows in pipe
#'
#' @param dat
#' @param colname
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
row_sum <- function(dat, colname, ...) {

  quo_cols <- quos(...)
  quo_new  <- enquo(colname)

  dat %>% mutate(!!quo_name(quo_new) := dat %>%
                   select(!!!quo_cols) %>% rowSums(na.rm = TRUE))

}



