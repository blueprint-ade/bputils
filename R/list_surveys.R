
#' List surveys on the license, arrange by search term
#'
#' @return
#' @export
#' @import sodium qualtRics tibble dplyr
#'
#' @examples
list_surveys <- function(search = "") {

  qualtRics::registerOptions(
    api_token = decrypt_token("Z:/R/qxk.rds"),
    root_url = "ca1.qualtrics.com")

  res <- arrange(
    mutate_if(
      tibble::as_tibble(qualtRics::getSurveys()),
      is.factor, as.character),
    lastModified)

  Sys.unsetenv(c("QUALTRICS_API_KEY", "QUALTRICS_ROOT_URL"))

  if(search == "") {

    return(res)

  } else {

    dplyr::select(
      dplyr::arrange(
        dplyr::mutate(res,
          sd = stringdist::stringdist(tolower(search), tolower(name),
            weight = c(i = 0.01, d = 1, s = 0.9, t = 0.1))), sd), -sd)

  }

}
