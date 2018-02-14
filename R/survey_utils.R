#' A test for Jose.
#'
#' @return
#' @export
#'
#' @examples
jose_test <- function() {

  get_survey("SV_ekyHsGV6rs15Bc1")


}


#' register API KEY and root URL in env variables
#'
#'
#' @return
#' @export
#'
#' @examples
register_options <- function() {
  Sys.setenv(QUALTRICS_ROOT_URL = "ca1.qualtrics.com")
  Sys.setenv(QUALTRICS_API_KEY = decrypt_token("Z:/R/qxk.rds"))
}


#' Create payload for post request body
#'
#' @param id
#' @param format
#' @param labs
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
create_payload <- function(id, format = "csv", labs = FALSE, ...) {


  list(format = format,
       surveyId = id,
       useLabels = labs,
       ...) %>%
    jsonlite::toJSON(auto_unbox = TRUE) %>%
    stringr::str_replace("\"false\"", "false") %>%
    stringr::str_replace("\"true\"", "true")
}


#' Get an export of responses from a survey
#'
#' @param id
#' @param con
#' @param format
#' @param labs
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_survey <- function(id, folder = "Z:/R/temp", fname = "qxre.zip", format = "spss", labs = FALSE, ...) {

  if(Sys.getenv("QUALTRICS_API_KEY") == "") {

    register_options()

  }

  pl <- create_payload(id = id, format = format, labs = labs, ...)


  root_url <- paste0(Sys.getenv("QUALTRICS_ROOT_URL"), "/API/v3/responseexports")


  post_content <-httr::VERB("POST",
    url = root_url,
    httr::add_headers(headers()),
    body = pl) %>%
    httr::content()


  check_url <- paste0(root_url, "/", post_content$result$id)

  check_request <- httr::VERB("GET", url = check_url, httr::add_headers(headers()))
  print(check_request)

  file_url <- paste0(check_url, "/file")

  progress <- 0

  while(progress < 100) {
    cat("progress: \n")
    check_request <- httr::VERB("GET", url = check_url, httr::add_headers(headers())) %>%
      httr::content()
    p <- check_request$result$percentComplete
    cat(p, "\n")
    progress <- p


  }

  req <- httr::GET(file_url, httr::add_headers(headers()))

  cat("get status: ", req$status_code, "\n")

  if(req$status_code == 404) {

    cat("Qualtrics can't find that ID right now, trying again...\n")
    get_survey(id, folder = folder, fname = fname, format = format, labs = labs, ...)

  }

  con = paste0(folder, "/", fname)



  writeBin(req$content, con = con)

  cat("zip file saved to", con, "\n")

  archive <- unzip(con, exdir = folder)

  cat("extracted to", archive, "\n")

  res <- haven::read_spss(archive) %>%
    haven::as_factor()

  cat("file extracted", "\n")

  list.files(folder, full.names = TRUE) %>%
    map(file.remove)

  cat("cleanup complete")


  return(res)

}




#' Return API request headers
#'
#' @return
#' @export
#'
#' @examples
headers <- function() {

  c(
    'X-API-TOKEN' = Sys.getenv("QUALTRICS_API_KEY"),
    'Content-Type' = "application/json",
    'Accept' = '*/*',
    'accept-encoding' = 'gzip, deflate'
  )


}

