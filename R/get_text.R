#' Retrieve text of a node given a css attribute or an XPath request.
#'
#' @param html Either a document, a node set or a single node.
#' @param css,xpath Use either a css attribute or an XPath 1.0 request (not both).
#' @param node If several nodes match your selection, select the one you want. Defaults to 1 (first node).
#'
#' @return The text of the node.
#'
#' @examples
#' html %>% get_text(".nationality")
#'
#' film %>% get_text(xpath = "//*[contains(@class, 'actors')]//span[last()]")

get_text <- function(html, css, xpath, node = 1) {
  if(rlang::is_missing(html)) {
    stop('"html" argument is missing')
  }

  if(rlang::is_missing(css) && rlang::is_missing(xpath)) {
    stop('please provide either a css attribute or an xpath request')
  }

  if(!rlang::is_missing(css) && !rlang::is_missing(xpath)) {
    stop('get_text requires either a css attribute or an xpath request. Please choose one!')
  }

  # Prepare arguments as a list
  if(!rlang::is_missing(css)) {
    args <- list(css = css)
  } else {
    args <- list(xpath = xpath)
  }

  h <- rlang::exec(rvest::html_nodes, !!!args, x = html)

  # If no nodes are found, return NA_character_
  if(length(h) == 0) {
    return(NA_character_)
  }

  # Check if there are enough nodes
  if(node > length(h)) {
    stop(glue::glue('could only match {length(h)} nodes. Please use a lower node value!'))
  }

  h %>%
    .[node] %>%
    rvest::html_text() %>%
    # Clean the text: remove unwanted new lines, repeated spaces and so on.
    stringr::str_squish()
}
