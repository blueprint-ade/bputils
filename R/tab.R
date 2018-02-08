
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
tab <- function(dat, x, y, n = FALSE,
                col = FALSE, row = FALSE, cell = FALSE,
                row_p = FALSE, print = TRUE, total_row = FALSE,
                chisq = TRUE) {

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


  rep_count <- 7

  if(!print) {tbl <- tbl %>% select(-starts_with("print")); rep_count <- rep_count - 1}
  if(!row_p) {tbl <- tbl %>% select(-starts_with("r_p")); rep_count <- rep_count - 1}
  if(!n)     {tbl <- tbl %>% select(-starts_with("n_")); rep_count <- rep_count - 1}
  if(!col)   {tbl <- tbl %>% select(-starts_with("col")); rep_count <- rep_count - 1}
  if(!row)   {tbl <- tbl %>% select(-starts_with("row")); rep_count <- rep_count - 1}
  if(!cell)  {tbl <- tbl %>% select(-starts_with("cell")); rep_count <- rep_count - 1}

  if(!total_row) {tbl %<>% filter(!!quo_y != "Total")}


  tbl

}


#' Calculate chisq and out-put a dataframe
#'
#' @param dat results of a tablulation
#'
#' @return chisq tag
#' @export
#'
#' @examples
calc_chisq <- function(x, y) {

  x <- chisq.test(x, y)



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
