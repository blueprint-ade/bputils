# get_mailing_list <- function(list_id) {
#
#   root_url <- Sys.getenv("QUALTRICS_ROOT_URL")
#
#   ml_url <- appendRootUrl(root_url, list_id, type = "mailinglists")
#
#   x <- qualtricsApiRequest("GET", ml_url)
#
#   res <- x$result$elements %>% map(~()))
#
#
#
#   res
#
# }
