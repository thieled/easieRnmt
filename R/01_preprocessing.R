#' @title Detect Languages with fastText
#'
#' @description Identifies the language of each text in a vector or data.frame using fastText's
#' pretrained language identification model. Cleans problematic inputs (line breaks, control chars, invalid bytes).
#' Returns a data.table with row ids, input text, detected language, and confidence.
#'
#' @param x Character vector or data.frame containing texts to identify.
#' @param text_col Character, name of the text column if \code{x} is a data.frame. Ignored if \code{x} is a vector.
#' @param id_col Character, optional column in data.frame with unique ids. If NULL, sequential row ids are used.
#' @param model_path Character, path to the fastText lid.176.bin model. Defaults to ~/.cache/easieRnmt/lid.176.bin.
#' @param prob_threshold Numeric, threshold below which the detected language is replaced by "und". Default = 0.25.
#' @param und_label Character, label for undefined language. Default = "und".
#' @param threads Integer, number of threads for fastText. Default = parallel::detectCores().
#' @param verbose Logical, whether to print progress messages.
#'
#' @return A data.table with columns: \code{row_id}, \code{text}, \code{lang}, \code{lang_prob},
#' and optionally the user-provided id column.
#' @export
detect_languages_fasttext <- function(x,
                                      text_col = "text",
                                      id_col = NULL,
                                      model_path = NULL,
                                      prob_threshold = 0.25,
                                      und_label = "und",
                                      threads = parallel::detectCores(),
                                      verbose = TRUE) {
  if (!requireNamespace("fastText", quietly = TRUE)) stop("Package 'fastText' must be installed.")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' must be installed.")
  vmessage <- function(...) if (verbose) message(...)

  # Normalize input ---------------------------------------------------------
  dt <- if (is.character(x)) {
    data.table::data.table(row_id = seq_along(x), text = x)
  } else if (is.data.frame(x)) {
    if (!text_col %in% names(x)) stop("text_col not found in data.frame")
    out <- data.table::as.data.table(x)
    out[, row_id := .I]
    data.table::setnames(out, text_col, "text")
    if (!is.null(id_col) && id_col %in% names(out)) {
      data.table::setnames(out, id_col, "id")
    }
    out
  } else {
    stop("x must be a character vector or data.frame")
  }

  # Model path --------------------------------------------------------------
  if (is.null(model_path)) {
    home <- path.expand("~")
    cache_dir <- file.path(home, ".cache", "easieRnmt")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    model_path <- file.path(cache_dir, "lid.176.bin")
  }
  if (!file.exists(model_path)) {
    vmessage("Downloading fastText language identification model to ", model_path)
    utils::download.file(
      url = "https://dl.fbaipublicfiles.com/fasttext/supervised-models/lid.176.bin",
      destfile = model_path,
      mode = "wb"
    )
  }

  # Clean text --------------------------------------------------------------
  dt[, text := enc2utf8(text)]
  dt[is.na(text), text := ""]
  dt[, text := trimws(text)]
  dt[, text := gsub("[\r\n]+", " ", text)]
  dt[, text := gsub("[[:cntrl:]]", " ", text)]
  dt[, text := iconv(text, from = "", to = "UTF-8", sub = " ")]
  dt[nchar(text) > 5000, text := substr(text, 1, 5000)]
  dt[!grepl("[[:alpha:]]", text), text := ""]

  empty_idx <- which(dt$text == "")
  texts_for_ft <- dt$text
  texts_for_ft[empty_idx] <- " "  # placeholder for fastText

  # Run fastText ------------------------------------------------------------
  vmessage("Running fastText language detection...")
  ft_res <- tryCatch(
    fastText::language_identification(
      texts_for_ft,
      pre_trained_language_model_path = model_path,
      threads = threads
    ),
    error = function(e) {
      stop("fastText failed: ", conditionMessage(e))
    }
  )

  if (nrow(ft_res) != nrow(dt)) {
    stop("fastText returned ", nrow(ft_res), " rows for ", nrow(dt),
         " inputs. Likely cause: unhandled special characters.")
  }

  # Merge results -----------------------------------------------------------
  dt[, `:=`(
    lang = ft_res$iso_lang_1,
    lang_prob = ft_res$prob_1
  )]

  # Force "und" for empty inputs
  if (length(empty_idx) > 0) {
    dt[empty_idx, `:=`(lang = und_label, lang_prob = 0)]
  }

  # Apply threshold
  dt[lang_prob < prob_threshold, lang := und_label]

  return(dt[])
}
