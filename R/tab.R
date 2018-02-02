
#' Workflow / print / NSE friendly implementation of table()
#'
#' @param dat
#' @param x
#' @param y
#' @param col
#' @param row
#' @param cell
#'
#' @return
#' @export
#'
#' @examples
tab <- function(dat, x, y, n = TRUE,
                col = FALSE, row = FALSE, cell = FALSE,
                row_p = FALSE, print = FALSE, total_row = FALSE,
                chisq = FALSE) {

  quo_x <- enquo(x)
  quo_y <- enquo(y)

  dat %<>% select(!!quo_x, !!quo_y) %>% mutate_all(as.character)

  add_row_p <- function(dat) dat %>% mutate(r_p = round(r_total / max(r_total), 2))


  tbl <- dat %>%
    add_total(!!quo_y) %>%
    group_by(!!quo_x, !!quo_y) %>%
    summarize(n = n()) %>%
    group_by(!!quo_x) %>% mutate(c_total = sum(n)) %>%
    group_by(!!quo_y) %>% mutate(r_total = sum(n)) %>%
    ungroup() %>% mutate(N = sum(n)) %>%
    mutate(cell = n / N,
           col = n / c_total,
           row = n / r_total) %>%
    round_row(!!quo_y) %>%
    mutate(print = paste0(n, " ( ", scales::percent(row), " )")) %>%
    gather(var, val, n, cell:print) %>%
    unite(m, var, !!quo_x) %>%
    ungroup() %>%
    add_row_p %>%
    select(m, !!quo_y, val, r_total, r_p) %>%
    spread(m, val) %>%
    mutate_at(vars(starts_with("print")), funs(ifelse(is.na(.), "  -  ", .)))

  if(chisq) {

    tbl[is.na(tbl)] <- 0
    chisq <- calc_chisq(tbl %>% select_if(is.numeric))

  } else {

    chisq = c(-99, -99)

  }


  rep_count <- 7

  if(!print) {tbl <- tbl %>% select(-starts_with("print")); rep_count <- rep_count - 1}
  if(!row_p) {tbl <- tbl %>% select(-starts_with("r_p")); rep_count <- rep_count - 1}
  if(!n)     {tbl <- tbl %>% select(-starts_with("n_")); rep_count <- rep_count - 1}
  if(!col)   {tbl <- tbl %>% select(-starts_with("col")); rep_count <- rep_count - 1}
  if(!row)   {tbl <- tbl %>% select(-starts_with("row")); rep_count <- rep_count - 1}
  if(!cell)  {tbl <- tbl %>% select(-starts_with("cell")); rep_count <- rep_count - 1}

  if(!total_row) {tbl %<>% filter(!!quo_y != "Total")}




  tbl_print <- tbl %>%
    select(!!quo_y, r_total, starts_with("r_p"), starts_with("n"), everything())

  kbl <- kablify(tbl_print)

  len <- nchar(kbl[1])
  dsc <- str_c("| Table of ", quo_name(quo_x), " ~ ", quo_name(quo_y))
  dsc <- str_c(dsc, str_c(rep(" ", len - nchar(dsc) - 1), collapse = ""), "|")
  chi <- str_c("| \u03C7 = ", round(chisq[1], 1), " p = ", round(chisq[2], 5))
  chi <- str_c(chi, str_c(rep(" ", len - nchar(chi) - 1), collapse = ""), "|")

  header <- str_c(dsc, "\n", chi)

  cat(str_c("|:", str_c(rep("-", nchar(kbl[1]) - 4), collapse = ""), ":|"),
      header,
      str_c("|:", str_c(rep("-", nchar(kbl[1]) - 4), collapse = ""), ":|\n\n"),
      kbl,
      sep = "\n")


  invisible(tbl)

}


#' Calculate chisq and out-put a dataframe
#'
#' @param dat results of a tablulation
#'
#' @return chisq tag
#' @export
#'
#' @examples
calc_chisq <- function(dat) {

  x <- dat %>% ungroup() %>%
    select(starts_with("n_")) %>%
    chisq.test()


  tag <- data_frame(stat = x$statistic, p_val = x$p.value)



  return(tag)

}

#' Add total data for tab
#'
#' @param dat
#' @param x
#'
#' @return
#' @export
#'
#' @examples
add_total <- function(dat, x) {

  quo_x <- enquo(x)

  dat %>% bind_rows(
    dat %>% mutate(
      !!quo_name(quo_x) := "Total"
    )
  )


}


#' Correct rounded row totals to
#'
#' @param dat
#' @param y
#'
#' @return
#' @export
#'
#' @examples
round_row <- function(dat, y) {

  quo_y = enquo(y)

  dat %>% mutate(
    trunc = floor(row * 100),
    err = row * 100 - trunc) %>%
    arrange(!!quo_y, -err) %>%
    group_by(!!quo_y) %>%
    mutate(row = vec_correct(trunc) / 100) %>%
    select(-trunc, -err)

}

#' Generate a vector to correct rounding errors
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
vec_correct <- function(x) {


  err <- 100 - sum(x)

  len <- length(x)

  os <- len - err

  y <- c(rep(err, err), rep(0, os)) / ifelse(err == 0, 1, abs(err))

  x <- ifelse(is.na(x), y, x + y)

  x

}



tab_test <- function(dat, x, y, n = TRUE, col = FALSE, row = FALSE, cell = FALSE, row_p = FALSE) {

  quo_x <- enquo(x)
  quo_y <- enquo(y)

  dat %<>% select(!!quo_x, !!quo_y) %>% mutate_all(as.character)

  add_row_p <- function(dat) dat %>% mutate(r_p = round(r_total / max(r_total), 2))


  tbl <- dat %>%
    add_total(!!quo_y) %>%
    group_by(!!quo_x, !!quo_y) %>%
    summarize(n = n()) %>%
    group_by(!!quo_x) %>% mutate(c_total = sum(n)) %>%
    group_by(!!quo_y) %>% mutate(r_total = sum(n)) %>%
    ungroup() %>% mutate(N = sum(n)) %>%
    mutate(cell = n / N,
           col = n / c_total,
           row = n / r_total)

  tbl
}

