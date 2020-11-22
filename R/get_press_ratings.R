#' Return the ratings of a film as a tibble.
#'
#' @param url The ratings URL for a film on Allocine. It must match the right type of page / url : "^https://www.allocine.fr/film/fichefilm-.*/critiques/presse".
#'
#' @return The ratings of the film as a tibble.
#'
#' @examples
#' "https://www.allocine.fr/film/fichefilm-25802/critiques/presse/" %>%
#'   get_press_ratings()

get_press_ratings <- function(url) {
  if(rlang::is_missing(url)) {
    stop('no URL was provided :( Please specify the "url" argument!')
  }

  if(!stringr::str_detect(url, "^https://www.allocine.fr/film/fichefilm-.*/critiques/presse")) {
    warning('the provided URL does not match a proper film page on the allocine website. Returning NAs.')
    return(tibble::tibble(
      paper = NA_character_,
      rating = NA_character_
    ))
  }

  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = "//div[contains(@class, 'reviews-press-list')]//li[contains(@class, 'item')]") %>%
    purrr::map_dfr(~ tibble::tibble(
      paper = .x %>% rvest::html_nodes("span") %>% rvest::html_text() %>% .[2],
      rating = .x %>% rvest::html_nodes("span") %>% rvest::html_attr("title") %>% .[1]
      ))
}
