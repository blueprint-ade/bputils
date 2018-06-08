bars <- function(dat, x, y, f, fill) {

  x <- enquo(x)
  y <- enquo(y)
  f <- enquo(f)
  fill <- enquo(fill)

  ggplot(dat, aes(!!x, !!y)) +
    geom_col(aes(fill = !!fill)) +
    facet_wrap(vars(!!f)) +
    scale_fill_viridis_c()



}



get_unique_scales <- function(dat) {

  fcts <- dat %>% map(
    ~levels(.) %>%
      tolower %>%
      str_replace_all("[:space:]", " ") %>%
      trimws("both")
  ) %>% unique()

  scls <- fcts %>%
    map(~str_c("1-", first(.), "-", length(.), "-", last(.)))

  data_frame(scale_name = scls, scale_vals = fcts)

}

matches_scale <- function(col, scl) {

  scl <- scl %>% unlist()

  levels <- col %>%
    levels() %>%
    tolower() %>%
    str_replace_all("[:space:]", " ") %>%
    trimws("both")

  all(levels %in% scl)

}

select_scale <- function(dat, scl) {

  dat %>% select_if(~matches_scale(., scl))

}

select_scales <- function(dat, scls) {

  scls %>% mutate(data = scale_vals %>% map(~select_scale(dat, .x)))

}

plot_scales <- function(scale_data) {

  scale_data %>% mutate(
    long = data %>% map2(scale_vals, ~.x %>% gather(var, val) %>%
                        mutate(var = fct_relevel(var, .y, after = Inf))),
    p = long %>% map(~.x %>% ggplot(aes(val)) +
                       geom_bar() +
                       facet_wrap(~var) +
                       coord_flip())
  )


}

