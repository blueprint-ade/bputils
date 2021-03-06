#' Load fonts for win, pdf, and postscript devices
#'
#' @return
#' @export
#' @import extrafont
#'
#' @examples
fonts <- function() {
  extrafont::loadfonts(device = "win", quiet = TRUE)
  extrafont::loadfonts(device = "pdf", quiet = TRUE)
  extrafont::loadfonts(device = "postscript", quiet = TRUE)
}


#' Bind a folder of separate data files with the same headings into one object
#' takes excel, stata, spss, fixed width files, and standard table formats
#'
#' @param path the path to the folder where the data files are kept
#' @param format valid formats are csv, xls, dta, tsv, sav
#' @param col_widths only used for reading fixed width files
#'
#' @return bound data from
#' @export
#'
#' @examples

folder_bind <- function(dir_raw, format = "csv", col_widths = NULL) {

  read_func <- get_reader(format)
  used_file_names <- list_used_files(dir_raw, format)
  nfiles     <- length(used_file_names)

  df_bound <- foreach(i = 1:nfiles, .combine=rbind) %do% {
    read_func(paste0(dir_raw, used_file_names[i]))
  }

  df_bound

}


#' batch convert a folder of dataframes into individual stata files with
#' the same names
#'
#' @param dir_raw path to directory with files to be converted
#' @param dir_stata path to stata files directory
#' @param format valid formats are csv, xls, dta, tsv, sav
#' @param col_widths only used for reading fixed width files
#'
#' @return
#' @export
#'
#' @examples
folder_2stata <- function(dir_raw, dir_stata,
                          format = "csv", col_widths = NULL) {

  read_func <- get_reader(format)
  used_file_names <- list_used_files(dir_raw, format)

  for(file_name in used_file_names) {
    df_i  <- read_sav(paste0(dir_raw, file_name))
    new_name <- paste0(drop_extension(file_name), ".dta")
    haven::write_dta(df_i, paste0(dir_stata, new_name), version = 13)
    rm(df_i)
    gc()

    print(paste0(file_name, " converted, saved as ", new_name))
  }
}



#' Get a reader function for a file format
#'
#' @param format
#'
#' @return reader_function
#' @export
#'
#' @examples

get_reader      = function(format) {
  switch(
    format,
    csv = function(x, ...) read_csv(x, ...),
    xls = function(x) read_raw_excel(x),
    dta = function(x, ...) read_stata(x, ...),
    tsv   = function(x, ...) read_csv(x, sep = "\t", ...),
    fwf   = function(x, ...) read_fwf(x, skip = 0, fwf_widths(col_widths)),
    sav   = function(x, ...) read_sav(x, ...)
  )
}

#' Get list of files to be processed
#'
#' @param dir_path string: path to the directory of files to filter
#' @param format string: format of files to filter for
#'
#' @return character vector of files
#' @export
#'
#' @examples
list_used_files = function(dir_raw, format) {
  if(!(format %in% c("csv", "xls", "dta", "tsv", "sav"))) {
    stop(paste0("format \"", format, "\" not supported, supported formats are:",
                "\n\t- csv\n\t- xls\n\t- dta\n\t- tsv\n\t- sav"))
  }

  all_file_names <- list.files(dir_raw)
  used_file_names <- all_file_names[
    grepl(paste0(format, "$"), all_file_names)]
  print(paste("Files used: \n\t", used_file_names, sep = ",\n\t"))

  return(used_file_names)
}

#' Remove 3 letter extension from file name string
#'
#' @param file_name
#'
#' @return
#' @export
#'
#' @examples
drop_extension = function(file_name) {
  name_length <- nchar(file_name) - 4
  raw_file_name <- substr(file_name, 1, name_length)

  return(raw_file_name)
}

#' Automatically detect excel header
#'
#' @param file_name
#'
#' @return
#' @export
#'
#' @examples
read_raw_excel = function(file_name) {
  raw_excel <- readxl::read_excel(file_name, sheet = 1, col_names = FALSE)
  for(i in 1:nrow(raw_excel)) {
    nacount <- sum(is.na(raw_excel[i, ]))
    if(nacount == 0) {
      header_index <- i
      break
    }
  }
  colnames(raw_excel) <- raw_excel[header_index, ]
  clean_excel <- raw_excel[(header_index + 1): nrow(raw_excel), ]

  return(clean_excel)
}


