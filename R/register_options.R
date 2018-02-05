#' Wrapper for qualtRics::registerOptions which includes
#' our root url and the location of the encrypted API key
#'
#' @return
#' @export
#'
#' @examples
register_options <- function() {
  qualtRics::registerOptions(
    api_token = decrypt_token("Z:/R/qxk.rds"),
    root_url = "ca1.qualtrics.com")
}


