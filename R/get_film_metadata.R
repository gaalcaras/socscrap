#' Return the metadata of a film as a tibble.
#'
#' @param url The film page URL on Allocine. It must match the right type of page / url : "https://www.allocine.fr/film/fichefilm_gen_cfilm=.*".
#'
#' @return The metadata of the film as a tibble.
#'
#' @examples
#' "https://www.allocine.fr/film/fichefilm_gen_cfilm=25802.html" %>%
#'   get_film_metadata()

get_film_metadata <- function(url) {
  if(rlang::is_missing(url)) {
    stop('no URL was provided :( Please specify the "url" argument!')
  }

  if(!stringr::str_detect(url, "^https://www.allocine.fr/film/fichefilm_gen_cfilm=")) {
    warning('the provided URL does not match a proper film page on the allocine website. Returning NAs.')
    return(tibble::tibble(
      title = NA_character_,
      date = lubridate::NA_Date_,
      duration = NA_real_,
      genre = NA_character_,
      nationality = NA_character_,
      direction = NA_character_,
      actors = NA_character_
      ))
  }

  html <- xml2::read_html(url)

  tibble::tibble(
    title = html %>% get_text(".titlebar-title"),
    date = html %>% socscrap::get_text(".date") %>% lubridate::dmy(locale = "fr_FR.utf8"),
    duration = html %>%
      socscrap::get_text(xpath = "//*[contains(@class, 'meta-body-info')]/text()", node = 4) %>%
      lubridate::duration(units = "minutes") %>%
      as.numeric("minutes"),
    genre = html %>% socscrap::get_text(xpath = "//*[contains(@class, 'meta-body-info')]//span[last()]"),
    nationality = html %>% socscrap::get_text(".nationality"),
    direction = html %>% socscrap::get_text(".meta-body-direction"),
    actors = html %>% socscrap::get_text(".meta-body-actor")
  )
}
