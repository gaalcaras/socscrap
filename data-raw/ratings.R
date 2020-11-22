library(tidyverse)
library(purrr)

ratings <- list.files(here::here("data-raw", "ratings", "by-year")) %>%
  map_df(~ read_csv(here::here("data-raw", "ratings", "by-year", .x))) %>%
  mutate(rating = case_when(
    rating == "Chef-d'oeuvre" ~ 5,
    rating == "Très bien" ~ 4,
    rating == "Pas mal" ~ 3,
    rating == "Pas terrible" ~ 2,
    rating == "Très mauvais" ~ 1,
    is.na(rating) ~ NA_real_
  )) %>%
  mutate(direction = str_remove(direction, "De ")) %>%
  mutate(actors = str_remove(actors, "Avec ")) %>%
  mutate(nationality = str_to_title(nationality))

usethis::use_data(ratings, overwrite = TRUE)
