#' Create a Madlib
#'
#' This function creates a madlib based on a theme and user-provided words.
#'
#' @param theme_name The name of the theme to use.
#' @param words A named list of words to fill in the madlib.
#' @return A completed madlib.
#' @export
create_madlib <- function(theme_name, words) {
  theme <- load_theme(theme_name)
  template <- theme$select_template()
  madlib <- fill_in_words(template, words)
  return(madlib)
}

#' Fill in Words in Madlib
#'
#' This helper function fills in the words in the madlib template.
#'
#' @param template A string representing the madlib template.
#' @param words A named list of words to fill in the template.
#' @return A string with the words filled in.
fill_in_words <- function(template, words) {
  for (key in names(words)) {
    template <- gsub(paste0("\\{", key, "\\}"), words[[key]], template)
  }
  return(template)
}

#' Load a Madlib Theme
#'
#' This helper function loads a madlib theme.
#'
#' @param theme_name The name of the theme to load.
#' @return A list containing the madlib template selection function.
load_theme <- function(theme_name) {
  if (theme_name == "CalPoly") {
    source("R/calpoly_theme.R")
    return(calpoly_theme)
  } else if (theme_name == "Graduation") {
    source("R/graduation_theme.R")
    return(graduation_theme)
  } else {
    stop("Unknown theme.")
  }
}
