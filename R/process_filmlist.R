#' Return ratings from all the films in a given page.
#'
#' @param url The URL for a list of films on Allocine. It must match the right type of page / url : "^https://www.allocine.fr/films/decennie-\\d{4}/annee-\\d{4}/"".
#'
#' @return The ratings and the metadata of the films as a tibble.
#'
#' @examples
#' "https://www.allocine.fr/films/decennie-1990/annee-1999/?page=1" %>%
#'   process_filmlist()

process_filmlist <- function(url) {
  if(rlang::is_missing(url)) {
    stop('no URL was provided :( Please specify the "url" argument!')
  }

  if(!stringr::str_detect(url, "^https://www.allocine.fr/films/.*decennie-\\d{4}/annee-\\d{4}/")) {
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

  message(glue::glue("Processing page ",
                     stringr::str_extract(url,"\\d+$"),
                     "... ({url})"))

  url %>%
    xml2::read_html() %>%
    # Get <a> of films that have a press rating average
    rvest::html_nodes(xpath = "//span[contains(text(), 'Presse')]/parent::div/parent::div/parent::div/parent::div/parent::li//a[contains(@href, 'fichefilm')]") %>%
    rvest::html_attr("href") %>%
    # Manually generating press ratings URLs for each film
    stringr::str_replace(".*cfilm=(\\d*)\\.html$", "https://www.allocine.fr/film/fichefilm-\\1/critiques/presse/") %>%
    purrr::map_dfr(
      purrr::possibly(~ socscrap::get_film_ratings(.x),
                      otherwise = tibble::tibble(
                        paper = NA_character_,
                        rating = NA_character_,
                        title = NA_character_,
                        date = lubridate::NA_Date_,
                        duration = NA_real_,
                        genre = NA_character_,
                        nationality = NA_character_,
                        direction = NA_character_,
                        actors = NA_character_
                      )
      ))
}
