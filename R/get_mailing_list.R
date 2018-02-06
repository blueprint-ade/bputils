#' Pull information from a contact list. Currently
#' just for OBIP participants
#'
#' @param list_id
#'
#' @return
#' @export
#'
#' @examples
get_contacts <- function(list_id) {

  page_list <- get_page_list(list_id)

  page_list %>% map_df(~tabulate_request(.[1]))

}


#' List urls for each page of the contact list
#'
#' @param list_id
#'
#' @return
#' @export
#'
#' @examples
get_page_list <- function(list_id) {

  root_url <- Sys.getenv("QUALTRICS_ROOT_URL")
  ml_url <- appendRootUrl(root_url, list_id, type = "mailinglists")
  page_list <- list(ml_url)
  next_page <- function(url) {

    skip <- qualtricsApiRequest("GET", url)$result$nextPage
    print(skip)

    if(is.null(skip)) {

      return(page_list)

    }

    page_list <<- c(page_list, skip)
    next_page(skip)
  }


  next_page(ml_url)
}

#' Tabulate the results of a get request
#'
#' @param url
#'
#' @return
#' @export
#'
#' @examples
tabulate_request <- function(url) {

  get <- qualtricsApiRequest("GET", url)

  get$result$elements %>%
    map_df(~.[c("firstName", "lastName", "email", "id", "externalDataReference", "embeddedData")] %>% flatten())

}


#' plug null list references with "Missing"
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
pn <- function(x) {ifelse(is.null(x), "Missing", x)}


#' xcollect all mailing lists associated with teh
#'
#' @param search
#'
#' @return
#' @export
#'
#' @examples
get_mailing_lists <- function(search = NULL) {

  root_url <- Sys.getenv("QUALTRICS_ROOT_URL")
  url <- str_c(root_url, "/API/v3/mailinglists")

  df_lists <- qualtricsApiRequest("GET", url  = url)$result$elements %>%
    bind_rows()


  if(!is.null(search)) {

    df_lists %>% arrange(
      stringdist::stringdist(tolower(search), tolower(df_lists$name),
      weight = c(i = 0.01, d = 1, s = 0.9, t = 0.1)))

  } else {

    df_lists

  }



}



