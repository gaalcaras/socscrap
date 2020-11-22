library(socscrap)

url <- "https://www.allocine.fr/film/fichefilm-25802/critiques/presse/"

expected <- tibble::tibble(
  paper = c("Cahiers du Cinéma", "Le Journal du Dimanche", "Le Monde", "Libération", "Positif"),
  rating = "Chef-d'oeuvre"
)

nas <- tibble::tibble(
  paper = NA_character_,
  rating = NA_character_
)

test_that("using a valid url works fine", {
  expect_equal(get_press_ratings(url), expected)
})

test_that("using an invalid url returns NAs and warns the user", {
  expect_warning(out <- get_press_ratings("https://www.allocine.fr/film/fichefilm-25802/casting/"),
                 'the provided URL does not match a proper film page on the allocine website. Returning NAs.')
  expect_equal(out, nas)
})
