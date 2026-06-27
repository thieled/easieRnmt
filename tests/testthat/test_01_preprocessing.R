# clean_text tests
test_that("clean_text handles character vector input", {
  text <- c("Hello world", "Test text")
  result <- clean_text(text, verbose = FALSE)
  expect_is(result, "data.table")
  expect_true("text_clean" %in% names(result))
})

test_that("clean_text handles data.frame input", {
  df <- data.frame(text = c("Hello world", "Test text"), id = c(1, 2))
  result <- clean_text(df, text_col = "text", id_col = "id", verbose = FALSE)
  expect_is(result, "data.table")
  expect_true("id" %in% names(result))
})

test_that("clean_text returns character vector when return_string = TRUE", {
  text <- c("Hello world", "Test text")
  result <- clean_text(text, return_string = TRUE, verbose = FALSE)
  expect_is(result, "character")
})

test_that("clean_text truncates to max_char", {
  long_text <- paste(rep("a", 10000), collapse = "")
  result <- clean_text(long_text, max_char = 100, verbose = FALSE)
  expect_true(nchar(result$text_clean) <= 100)
})

test_that("clean_text tokenizes sentences", {
  text <- "First sentence. Second sentence. Third sentence."
  result <- clean_text(text, tokenize_sentences = TRUE, verbose = FALSE)
  expect_is(result, "data.table")
})

test_that("clean_text preserves doc_idx", {
  text <- c("First", "Second", "Third")
  result <- clean_text(text, verbose = FALSE)
  expect_true("doc_idx" %in% names(result))
})

test_that("clean_text rejects invalid text_col", {
  df <- data.frame(name = c("test"))
  expect_error(clean_text(df, text_col = "text", verbose = FALSE))
})

test_that("clean_text rejects invalid input type", {
  expect_error(clean_text(123, verbose = FALSE))
})

# detect_languages tests
test_that("detect_languages returns data.table with lang columns", {
  skip_if_not_installed("fastText")
  text <- c("Hello world", "Testing language")
  result <- detect_languages(text, verbose = FALSE)
  expect_is(result, "data.table")
  expect_true("lang" %in% names(result))
  expect_true("lang_prob" %in% names(result))
})

test_that("detect_languages handles single text", {
  skip_if_not_installed("fastText")
  result <- detect_languages("Hello world", verbose = FALSE)
  expect_equal(nrow(result), 1)
})

test_that("detect_languages preserves id column", {
  skip_if_not_installed("fastText")
  df <- data.frame(text = c("Hello", "Test"), id = c("doc1", "doc2"))
  result <- detect_languages(df, text_col = "text", id_col = "id", verbose = FALSE)
  expect_true("id" %in% names(result))
})

test_that("detect_languages handles data.frame input", {
  skip_if_not_installed("fastText")
  df <- data.frame(text = c("Hello world", "Another test"))
  result <- detect_languages(df, text_col = "text", verbose = FALSE)
  expect_is(result, "data.table")
})

# preprocess tests
test_that("preprocess returns list of data.tables", {
  skip_if_not_installed("fastText")
  text <- c("Hello world", "Another example")
  result <- preprocess(text, targ_lang = "en", verbose = FALSE)
  expect_is(result, "list")
  expect_true(all(sapply(result, is.data.table)))
})

test_that("preprocess handles data.frame input", {
  skip_if_not_installed("fastText")
  df <- data.frame(text = c("Hello", "Test"), id = c(1, 2))
  result <- preprocess(df, text_col = "text", id_col = "id", targ_lang = "en", verbose = FALSE)
  expect_is(result, "list")
})

test_that("preprocess returns non-empty list", {
  skip_if_not_installed("fastText")
  text <- c("Hello world", "Test message")
  result <- preprocess(text, targ_lang = "en", verbose = FALSE)
  expect_true(length(result) > 0)
})
