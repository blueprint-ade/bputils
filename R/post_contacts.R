#' Post a dataframe of new contacts to the contact list
#'
#' @param url
#' @param posts
#'
#' @return
#' @export
#'
#' @examples
post_contacts <- function(new_contacts, list_id) {


  root_url <- Sys.getenv("QUALTRICS_ROOT_URL")
  ml_url <- appendRootUrl(root_url, list_id, type = "mailinglists")

  if(!all(c("firstName", "lastName", "email", "externalDataRef", "Survey ID") %find% colnames(new_contacts))) {

    stop("new_contacts doesn't have the right columns.")

  }

  print(ml_url)

  posts <- generate_posts(new_contacts)

  map(posts, ~cat(.))

  posts %>% map(~httr::VERB("POST", url = ml_url, body = .))

}


#' Generate a list of json payloads for put requests
#'
#' @param df_new_ids
#'
#' @return
#' @export
#'
#' @examples
generate_posts <- function(df_new_ids) {

  df_new_ids %>%
    split(.$lastName) %>%
    map(~(
      list(
        firstName = .$firstName,
        lastName  = .$lastName,
        email     = .$email,
        externalDataRef = .$externalDataRef,
        embeddedData = list(
          `Survey ID` = .$`Survey ID`))
    ) %>% jsonlite::toJSON(auto_unbox = TRUE) %>%
      jsonlite::prettify() %>% str_sub(1, -1))

}





