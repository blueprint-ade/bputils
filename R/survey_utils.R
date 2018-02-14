#' A test for Jose.
#'
#' @return
#' @export
#'
#' @examples
jose_test <- function() {

  if(Sys.getenv("QUALTRICS_API_KEY") == "") {

    register_options()

  }


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

  pl <- create_payload(id = id, format = format, labs = FALSE)

  headers <-   c(
    'X-API-TOKEN' = Sys.getenv("QUALTRICS_API_KEY"),
    'Content-Type' = "application/json",
    'Accept' = '*/*',
    'accept-encoding' = 'gzip, deflate'
  )

  root_url <- paste0(Sys.getenv("QUALTRICS_ROOT_URL"), "/API/v3/responseexports")

  post_content <-httr::VERB("POST",
    url = root_url,
    httr::add_headers(headers),
    body = pl) %>%
    httr::content()

  file_url <- paste0(root_url, "/", post_content$result$id, "/file")

  req <- httr::GET(file_url, httr::add_headers(headers))

  con = paste0(folder, "/", fname)

  writeBin(req$content, con = con)

  res <- haven::read_spss(unzip(con, exdir = folder)) %>%
    haven::as_factor()

  list.files(folder, full.names = TRUE) %>%
    map(file.remove)


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

