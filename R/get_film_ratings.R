#' Return the ratings and the metadata of a film as a tibble.
#'
#' @param url The ratings URL for a film on Allocine. It must match the right type of page / url : "^https://www.allocine.fr/film/fichefilm-.*/critiques/presse".
#'
#' @return The ratings and the metadata of the film as a tibble.
#'
#' @examples
#' "https://www.allocine.fr/film/fichefilm-25802/critiques/presse/" %>%
#'   get_film_ratings()

get_film_ratings <- function(url) {
  if(rlang::is_missing(url)) {
    stop('no URL was provided :( Please specify the "url" argument!')
  }

  if(!stringr::str_detect(url, "^https://www.allocine.fr/film/fichefilm-.*/critiques/presse")) {
    warning('the provided URL does not match a proper film page on the allocine website. Returning NAs.')
    return(tibble::tibble(
      paper = NA_character_,
      rating = NA_character_,
      title = NA_character_,
      date = lubridate::NA_Date_,
      duration = NA_real_,
      genre = NA_character_,
      nationality = NA_character_,
      direction = NA_character_,
      actors = NA_character_
    ))
  }

  film_page_url <- glue::glue(
    "https://www.allocine.fr/film/fichefilm_gen_cfilm=",
    stringr::str_replace(url, ".*fichefilm-(\\d*)/.*", "\\1"),
    ".html"
    )

  url %>%
    socscrap::get_press_ratings() %>%
    dplyr::bind_cols(socscrap::get_film_metadata(film_page_url))
}
