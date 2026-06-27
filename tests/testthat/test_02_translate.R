# translate tests
test_that("translate handles character vector input", {
  skip_if_not_installed("fastText")
  text <- c("Hello world", "Test text")
  result <- translate(text, targ_lang = "en", verbose = FALSE)
  expect_is(result, "data.table")
})

test_that("translate handles data.frame input", {
  skip_if_not_installed("fastText")
  df <- data.frame(text = c("Hello world", "Test text"), id = c(1, 2))
  result <- translate(df, text_col = "text", id_col = "id", targ_lang = "en", verbose = FALSE)
  expect_is(result, "data.table")
  expect_true("id" %in% names(result))
})

test_that("translate returns data.table", {
  skip_if_not_installed("fastText")
  text <- c("Hello world", "Test message")
  result <- translate(text, targ_lang = "en", verbose = FALSE)
  expect_is(result, "data.table")
})

test_that("translate handles single text", {
  skip_if_not_installed("fastText")
  result <- translate("Hello world", targ_lang = "en", verbose = FALSE)
  expect_is(result, "data.table")
  expect_true(nrow(result) >= 1)
})

test_that("translate requires targ_lang parameter", {
  skip_if_not_installed("fastText")
  expect_error(translate("Hello world", verbose = FALSE))
})

test_that("translate preserves id column", {
  skip_if_not_installed("fastText")
  df <- data.frame(text = c("Hello", "Test"), id = c("doc1", "doc2"))
  result <- translate(df, text_col = "text", id_col = "id", targ_lang = "en", verbose = FALSE)
  expect_true("id" %in% names(result))
})
