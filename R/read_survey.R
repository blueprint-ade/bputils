#' Get response data using a qualtrics survey id
#'
#' @param survey_id
#'
#' @return
#' @export
#'
#' @importFrom getPass getPass
#' @importFrom qualtRics getSurvey registerOptions
#' @examples
read_survey <- function(survey_id, labs = FALSE, ...) {

  registerOptions(
    api_token = decrypt_token("Z:/R/qxk.rds"),
    root_url = "ca1.qualtrics.com")

  res <- tibble::as_tibble(getSurvey(
    surveyID = survey_id,
    save_dir = tempdir(),
    useLabels = labs,
    verbose = TRUE,
    force_request = TRUE, ...))

  Sys.unsetenv(c("QUALTRICS_API_KEY", "QUALTRICS_ROOT_URL"))

  return(res)

}


#' Decrypt token with password
#'
#' @param path
#'
#' @return
#' @export
#'
#' @importFrom getPass getPass
#' @import sodium
#' @examples
decrypt_token <- function(path) {


  pass   <- getPass::getPass()
  cipher <- readRDS(path)
  key    <- sodium::hash(charToRaw(pass))

  unserialize(sodium::data_decrypt(cipher, key))


}


unequal_cols <- function(rc, lab) {

  cols <- colnames(rc)

  diff_cols <- cols[lapply(cols,
      function(name) {!identical(rc[[name]], lab[[name]])}) %>% unlist()]

  df_fct <- lapply(diff_cols,
      function(x) {

        labs <- rc[[x]]
        names(labs) <- lab[[x]]

        forcats::as_factor(
          haven::labelled(rc[[x]], labs)
        )
      }
      ) %>% pipe_names(x) %>%
    bind_cols()
  return(df_fct)

}

pipe_names <- function(x, nms) {

  names(x) <- nms

  x
}




