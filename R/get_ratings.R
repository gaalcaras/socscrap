#' Return ratings from all the films in a given year.
#'
#' @param year The year as a numeric value, e.g. 1999.
#' @param pages Only retrieve ratings from certain pages instead of all of them.
#' @return The ratings and the metadata of the films as a tibble.
#'
#' @examples
#' get_ratings(2017)

get_ratings <- function(year, pages) {
  if(rlang::is_missing(year)) {
    stop('year was not provided :(')
  }

  if(!is.numeric(year) || year < 1900) {
    stop('year has to be a numeric value, such as 1999.')
  }

  year_url <- glue::glue("https://www.allocine.fr/films/alphabetique/decennie-{trunc(year/10)*10}/annee-{year}/")

  if(rlang::is_missing(pages)) {
    nb_pages <- year_url %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = "//*[contains(@class, 'button-md')][last()]") %>%
      rvest::html_text() %>%
      dplyr::last()
    this_year_urls <- purrr::map_chr(1:nb_pages, ~ glue::glue("{year_url}?page=", .x))
  } else {
    this_year_urls <- purrr::map_chr(pages, ~ glue::glue("{year_url}?page=", .x))
    nb_pages <- length(pages)
  }

  message(glue::glue("----- {year} ({nb_pages} pages) -----"))

  this_year_urls %>%
    purrr::map_dfr(
      purrr::possibly(~ socscrap::process_filmlist(.x),
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
                      ))
    )
}
