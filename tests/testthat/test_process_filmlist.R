library(socscrap)

url <- "https://www.allocine.fr/films/alphabetique/decennie-1990/annee-1999/?page=1"

nas <- tibble::tibble(
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

test_that("using a valid url works fine", {
  expect_equal(dim(process_filmlist(url)), c(52, 9))
  expect_equal(nrow(process_filmlist(url) %>% dplyr::count(title)), 4)
  expect_message(process_filmlist(url))
})

test_that("using an invalid url returns NAs and warns the user", {
  expect_warning(out <- process_filmlist("https://www.allocine.fr/film/fichefilm-25802/casting/"),
                 'the provided URL does not match a proper film page on the allocine website. Returning NAs.')
  expect_equal(out, nas)
})
