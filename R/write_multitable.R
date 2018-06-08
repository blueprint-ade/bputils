

#' Write a named list of tables to a csv file with headers
#'
#' @param dat
#' @param file_path
#'
#' @return
#' @export
#'
#' @examples
multi_table <- function(dat, file_path) {

  out <- function(name, dat) {

    cat(name)
    cat("\n\n")
    write.csv(dat, row.names = FALSE)
    cat("\n\n\n-------------------------------------------\n\n\n")

  }

  sink(file_path)

  names(dat) %>% map(. %>% out(dat[[.]]))

  sink()
}
