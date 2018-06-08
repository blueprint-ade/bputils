

#' make a brett-lamb style barbell chart, with either the whole
#' bar from 0 indicated, or just the difference.
#'
#' @param dat
#' @param x
#' @param xend
#' @param y
#' @param mapping
#' @param full_bar
#' @param pct
#'
#' @return
#' @export
#'
#' @examples
gg_barbell <- function(dat, x, xend, y, m,
                       full_bar = FALSE, pct = FALSE) {

  x <- enquo(x)
  y <- enquo(y)
  xend <- enquo(xend)

  d <- dat %>% select(!!x, !!xend) %>% unlist %>% as.numeric %>% range()
  xlim <- (d - d %% 10) + c(0, 10)


  if(full_bar) {

    xlim <- c(0, 100)

  }

  d <- dat %>%
    mutate(mv = map2_dbl(!!x, !!xend, ~min(.x, .y)))


  d %>% ggplot(aes(x = !!x, xend = !!xend, y = !!y, yend = !!y)) +
    geom_segment(aes(xend = min_val),
                 x = -Inf,
                 lty = 1,
                 lwd = ifelse(full_bar, 4, 1.4),
                 colour = "grey50",
                 alpha = 0.3) +
    geom_segment(m, lwd = 4) +
    geom_segment(aes(xend = !!x, y = as.numeric(!!y) + 0.4,
                     yend = as.numeric(!!y) -0.2)) +
    geom_segment(aes(x = !!xend, y = as.numeric(!!y) + 0.4,
                     yend = as.numeric(!!y) -0.2), lwd = 1.5) +
    annotate("segment",x=-Inf,xend=c(-Inf),y=Inf,yend=c(0),color="grey60",lwd=1) +
    scale_alpha_discrete(range = c(0.4, 1), guide = FALSE)+
    scale_x_continuous(breaks = function(x) {
      x <- x - x%%5 + c(-10, 10);
      seq(x[1], x[2], by = ifelse(full_bar, 10, 5))},
      limits = xlim,
      labels = ifelse(pct, function(x){ paste0(x, "%")},
                      function(x){x}),
      expand = c(0, 0)) +
    labs(x = NULL, y = NULL)
}




#' plot a variable as change across facets
#'
#' @param dat
#' @param x
#' @param y
#' @param m
#' @param f
#' @param lab
#' @param scales
#'
#' @return
#' @export
#'
#' @examples
gg_change <- function(dat, x, y, m, f, lab, scales) {

  x <- enquo(x)
  y <- enquo(y)
  f <- enquo(f)
  lab <- enquo(lab)


  dat %>%
    mutate(hj2 = !!y > 0) %>%
    ggplot(aes(!!x, !!y, xend = !!x)) +
    geom_segment(m,
                 yend = 0,
                 lwd = 4) +
    geom_segment(aes(yend = !!y,
                     x = as.numeric(!!x) + 0.4,
                     xend = as.numeric(!!x) -0.2),
                 lwd = 0.5) +
    geom_text(aes(label = !!lab, hjust = hj2),
              y = 0,
              family = "Roboto Condensed") +
    coord_flip() +
    facet_wrap(f, nrow = 1, scales = scales) +
    scale_y_continuous(expand = c(0.1,0.1)) +
    scale_alpha_discrete(range = c(0.4, 1), guide = FALSE) +
    labs(x = NULL, y = NULL) +
    annotate("segment",x=Inf,xend=c(Inf, -Inf),y=c(-Inf, 0),yend=c(Inf, 0),color="grey60",lwd=1)

}


gg_slope <- function(dat, time, value, group, colour,
                     buffer = 0.05, cross = FALSE,
                     fill = "white", shape = 21, size = 4, lwd = 2) {

  t <- enquo(time)
  v <- enquo(value)
  g <- enquo(group)
  colour <- enquo(colour)

  print(colour)

  o <- dat %>%
    filter(!!t == min(!!t)) %>%
    arrange(!!v) %>%
    mutate(o = row_number()) %>%
    select(!!g, o)

  if(cross) {

    d <- dat %>%
      arrange(!!v)

  } else {

    d <- dat %>%
      left_join(o) %>%
      arrange(!!t, o)

  }


  ms <- buffer * diff(range(d %>% pull(!!v)))

  d <- d %>% group_by(!!t) %>%
    mutate(lt = !!v - lag(!!v))

  if(cross) {

    d <- d %>%
      mutate(md = lt,
             ys = max(ms - md, 0))


  } else {
    d <- d %>%
      group_by(!!g) %>%
      mutate(md = min(lt),
             ys = max(ms - md, 0))

  }

  d <- d %>%
    group_by(!!t) %>%
    mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>%
    mutate(yt = cumsum(ys),
           yc = yt + !!v,
           hj = ifelse(!!t == 2011, 1, 0))

  nt <- quo_name(t)

  d <- d %>%
    ungroup() %>%
    mutate(
    lb = ifelse(!!t == 2011,
                str_c(!!g, " ", !!v),
                str_c(" ", !!v)),
    !!nt := as.numeric(!!t))

  p1 <- d %>% ungroup() %>%
    ggplot(aes(!!t, yc, group = !!g))

  p2 <- p1 +
    geom_line(aes(colour = !!colour), lwd = 1)

  p3 <- p2 +
    geom_text(aes(label = lb, hjust = hj), family = "Consolas")

  p4 <- p3 +
    scale_x_continuous(limits = c(2006, 2019),
                       breaks = c(2011, 2017)) +
    geom_point(aes(colour = !!colour),
               fill = "white", shape = 21, size = 4, lwd = 2)


  p4


}


