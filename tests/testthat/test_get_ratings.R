library(socscrap)

test_that("missing or invalid year argument fails gracefully", {
  expect_error(get_ratings(), "year was not provided :\\(")
  expect_error(get_ratings(98), 'year has to be a numeric value, such as 1999.')
  expect_error(get_ratings("deux mille dix"), 'year has to be a numeric value, such as 1999.')
})

test_that("valid year does work", {
  expect_message(get_ratings(2010, pages = c(2, 5)), "----- 2010 \\(2 pages\\) -----")
})
