#' Generate a new report
#'
#' @param title
#' @param filename
#' @param header
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
new_report <- function(title, folder, filename, header, ...) {

  write_n_log(
    c(header, ...),  folder, filename
  )

}


#' Make a header for a dashboard
#'
#' @param title
#'
#' @return
#' @export
#'
#' @examples
dash_header <- function(title) {

sprintf(
'---
title: \"%s\"
output:
    flexdashboard::flex_dashboard:
        orientation: columns
        vertical_layout: scroll
---', title)

}

#' Make a header for a word document
#'
#' @param title
#'
#' @return
#' @export
#'
#' @examples
docx_header <- function(title) {

  sprintf(
'---
title: \"%s\"
date: \"%s\"
output:
    word_document:
        reference_docx: "template.docx"
---', title, as.character(today()))

}




#' Print a list of tables as a writable vector, separated by third level headers
#'
#' @param tab_list
#'
#' @return
#' @export
#'
#' @examples
write_tab_list <- function(tab_list) {

  names(tab_list) %>%
    map(~c(paste0("\n\n### ", ., "\n\n"), tab_list[[.]] %>%
             kablify())) %>% unlist()

}


#' Make a set of tab functions to map onto any data with appropriate colnames
#'
#' @param dat
#' @param y
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
tabset <- function(y, ..., by = none) {

  by = enquo(by)
  vars <- quos(...)
  y <- enquo(y)

  fun <- function(dat) {

    vars %>% map(~. %>% tab(!!.x, !!y, n = FALSE, print = FALSE, row = TRUE, row_p = TRUE)) %>%
      map(~.(dat)) %>%
      set_names(vars %>% map(quo_name))

  }

  if(quo_name(by) == "none") {return(fun)} ##hard return if no by is specified




  . %>% split(.[[quo_name(by)]]) %>% map(fun)


}




tester <- function(args = list(n = FALSE, print = FALSE, row = TRUE, row_p = TRUE)) {



  mpg %>% tab(cyl, drv, args)




}


poop <- function(...) {

  x <- quos(...)
  y <- list(...)
  z <- quos(x = 5, y = 10)
  z_list <- quos(list(x = 5, y = 10))

  sumxy(UQ(y))

  # print(x)
  # print(y)
  # print(z)

}


