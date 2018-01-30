#' Build a kable from a table, change proportions to
#' percentages
#'
#' @param tab
#'
#' @return
#' @export
#'
#' @examples
kablify <- function(tab) {

  tab %>%
    mutate_if(
      function(x) ifelse(is.factor(x), FALSE, is.numeric(x) & max(x) <= 1 & min(x) >= 0),
      scales::percent
    ) %>%
    knitr::kable(digits = 3)



}
