library(socscrap)

film <- xml2::read_html("https://www.allocine.fr/film/fichefilm_gen_cfilm=25802.html")

test_that("forgetting mandatory arguments fails gracefully", {
  expect_error(get_text(css = ".date"), '"html" argument is missing')
  expect_error(film %>% get_text(), 'please provide either a css attribute or an xpath request')
})

test_that("providing conflicting arguments fails gracefully", {
  expect_error(film %>% get_text(css = ".date", xpath = "//div"), 'get_text requires either a css attribute or an xpath request. Please choose one!')
})

test_that("using css attribute works", {
  expect_equal(film %>% get_text(css = ".date"), "20 août 1980")
  expect_equal(film %>% get_text(css = ".meta-body-actor"), "Avec Mark Hamill, Harrison Ford, Carrie Fisher")
})

test_that("using xpath attribute works", {
  expect_equal(film %>% get_text(xpath = "//*[contains(@class, 'meta-body-info')]//span[last()]"), "Aventure")
})

test_that("using a node that does not exist returns NA", {
  expect_equal(film %>% get_text(css = ".actors"), NA_character_)
  expect_equal(film %>% get_text(xpath = "//*[contains(@class, 'actors')]//span[last()]"), NA_character_)
})

test_that("choosing a node to return works", {
  expect_equal(film %>% get_text(xpath = "//*[contains(@class, 'meta-body-info')]/text()", node = 4), "2h 04min")
})

test_that("choosing a node number that does not exist fails gracefully", {
  expect_error(film %>% get_text(css = ".date", node = 5), 'could only match 2 nodes. Please use a lower node value!')
})
