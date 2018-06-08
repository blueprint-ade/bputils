

null_recursive <- function(ls, name) {


  ls %>% map(~(
    if(typeof(.) == "list") {

      .[[name]] <- NULL
      map(., ~(null_recursive(., name)))

    } else {

      .

    }
  ))
}
