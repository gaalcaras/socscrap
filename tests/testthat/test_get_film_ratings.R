library(socscrap)

url <- "https://www.allocine.fr/film/fichefilm-25802/critiques/presse/"

expected <- tibble::tibble(
  paper = c("Cahiers du Cinéma", "Le Journal du Dimanche", "Le Monde", "Libération", "Positif"),
  rating = "Chef-d'oeuvre") %>%
  tibble::add_column(
    title = "Star Wars : Episode V - L'Empire contre-attaque",
    date = lubridate::ymd("1980-08-20"),
    duration = as.double(124),
    genre = "Aventure",
    nationality = "américain",
    direction = "De Irvin Kershner",
    actors = "Avec Mark Hamill, Harrison Ford, Carrie Fisher"
  )

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
  expect_equal(get_film_ratings(url), expected)
})

test_that("using an invalid url returns NAs and warns the user", {
  expect_warning(out <- get_film_ratings("https://www.allocine.fr/film/fichefilm-25802/casting/"),
                 'the provided URL does not match a proper film page on the allocine website. Returning NAs.')
  expect_equal(out, nas)
})
