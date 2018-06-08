#' Get age from dob, accounting for leap years
#'
#' @param dob
#' @param age_day
#' @param units
#' @param floor
#'
#' @return
#' @export
#'
#' @examples
age <- function(dob, age_day = today(), units = "years", floor = TRUE) {
  calc_age = interval(dob, age_day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc_age)))
  return(calc_age)
}
