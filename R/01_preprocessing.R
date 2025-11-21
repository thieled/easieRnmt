#' @title Replace Emojis with Names
#'
#' @description Replaces all emojis in a character vector with their textual names (in :name: style).
#'
#' @param text_vec Character vector of texts.
#'
#' @return Character vector with emojis replaced by names.
#' @export
replace_emoji_with_name <- function(text_vec) {
  if (!requireNamespace("emoji", quietly = TRUE)) {
    stop("Package 'emoji' is required for this function. Please install it.")
  }
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is required for this function. Please install it.")
  }

  emoji_vec <- emoji::emoji_name
  emoji_vec <- emoji_vec[grepl("[[:alpha:]]", names(emoji_vec))]

  # emoji → :name: mapping
  replacement_map <- stats::setNames(paste0(" [:", names(emoji_vec), ":] "), unname(emoji_vec))
  emoji_chars <- names(replacement_map)

  # Filter emojis that actually occur in text_vec (to avoid massive unnecessary replace)
  present_idx <- stringi::stri_detect_fixed(
    rep(paste(text_vec, collapse = " "), length(emoji_chars)),
    emoji_chars
  )
  emoji_chars <- emoji_chars[present_idx]
  replacements <- unname(replacement_map[emoji_chars])

  if (length(emoji_chars) == 0L) {
    return(text_vec)
  }

  # Vectorized replacement: replace all present emojis in one sweep
  text_vec <- stringi::stri_replace_all_fixed(
    str = text_vec,
    pattern = emoji_chars,
    replacement = replacements,
    vectorize_all = FALSE
  )

  text_vec
}



#' @title Clean, Normalize, and Tokenize Text
#'
#' @description Cleans and optionally tokenizes raw text data. Handles emoji replacement,
#' removal of unsupported characters, normalization, sentence tokenization, and
#' chunking into word-limited segments. Returns either a standardized data.table
#' or a character vector.
#'
#' @param x Character vector or data.frame containing texts to clean.
#' @param text_col Character, name of the text column if `x` is a data.frame.
#' Ignored if `x` is a character vector. Default = "text".
#' @param id_col Character, optional column name in the input data.frame to
#' preserve as `id`. Default = NULL.
#' @param lang_guess_col Character, optional column name in the input data.frame
#' to preserve as `lang_guess`. Default = NULL.
#' @param replace_emojis Logical, whether to replace emojis with placeholder
#' names. Default = TRUE.
#' @param replace_alphaless Logical, whether to replace strings that contain no
#' alphabetic characters with empty strings. Default = TRUE.
#' @param max_char Integer, maximum number of characters per text. Texts longer
#' than this are truncated. Default = 5000.
#' @param tokenize_sentences Logical, whether to split texts into sentences and
#' chunks. Default = TRUE.
#' @param max_words Integer, maximum number of words per sentence or chunk.
#' Long sentences are split into chunks of this size. Default = 50.
#' @param clean_characters Logical, whether to apply character normalization
#' (regex replacements, squishing, punctuation handling). Default = TRUE.
#' @param return_string Logical, if TRUE, return only the cleaned character
#' vector (`text_clean`) instead of a full data.table. Default = FALSE.
#' @param verbose Logical, whether to print progress messages. Default = TRUE.
#'
#' @return Either:
#' * A data.table with columns:
#'   - `sen_id`: Unique identifier for each sentence or chunk.
#'   - `doc_idx`: Row index of the original document in the input.
#'   - `sen_idx`: Sentence/chunk index within each document.
#'   - `id`: Optional user-provided identifier.
#'   - `text_orig`: Original text.
#'   - `text_clean`: Cleaned and normalized text.
#'   - `lang_guess`: Optional language guess column, if present.
#' * Or a character vector of cleaned texts if `return_string = TRUE`.
#'
#' @import data.table
#' @export
clean_text <- function(x,
                       text_col = "text",
                       id_col = NULL,
                       lang_guess_col = NULL,
                       replace_emojis = TRUE,
                       replace_alphaless = TRUE,
                       max_char = 5000,
                       tokenize_sentences = TRUE,
                       max_words = 50,
                       clean_characters = TRUE,
                       return_string = FALSE,
                       verbose = TRUE) {

  vmessage <- function(...) if (verbose) message(...)

  # --- Input handling ---
  if (is.character(x)) {
    dt <- data.table::data.table(doc_idx = seq_along(x), text_orig = x)
  } else if (is.data.frame(x)) {
    if (!text_col %in% names(x)) stop("text_col not found in data.frame")
    dt <- data.table::as.data.table(x)
    dt[, doc_idx := .I]
    data.table::setnames(dt, text_col, "text_orig")
    if (!is.null(id_col) && id_col %in% names(dt)) {
      data.table::setnames(dt, id_col, "id")
    }
    if (!is.null(lang_guess_col) && lang_guess_col %in% names(dt)) {
      data.table::setnames(dt, lang_guess_col, "lang_guess")
    }
  } else {
    stop("x must be a character vector or data.frame")
  }

  # --- Cleaning pipeline ---
  dt[, text_clean := enc2utf8(text_orig)]
  dt[is.na(text_clean), text_clean := ""]
  if (replace_emojis) dt[, text_clean := replace_emoji_with_name(text_clean)]
  dt[, text_clean := iconv(text_clean, from = "", to = "UTF-8", sub = " ")]
  dt[nchar(text_clean) > max_char, text_clean := substr(text_clean, 1, max_char)]
  if (replace_alphaless) dt[!grepl("[[:alpha:]]", text_clean), text_clean := ""]

  if (clean_characters) {
    dt[, text_clean := stringi::stri_replace_all_regex(
      text_clean, "[^\\p{L}\\p{N}\\p{P}\\p{Zs}]", " "
    )]
    dt[, text_clean := textclean::replace_curly_quote(text_clean)]
    dt[, text_clean := stringr::str_replace_all(text_clean, "(\\d)\\.(?=\\d)", "\\1 ")]
    dt[, text_clean := stringr::str_replace_all(text_clean, "(\\d)\\.(?!\\d)", "\\1 ")]
    dt[, text_clean := stringr::str_replace_all(text_clean, '\"{2,}', '"')]
    dt[, text_clean := stringr::str_replace_all(text_clean, "'{2,}", "'")]
    dt[, text_clean := stringr::str_squish(text_clean)]
  }

  # --- Sentence + chunk tokenization ---
  if (tokenize_sentences) {
    vmessage("Tokenizing texts into sentences and chunks...")

    tokenized_list <- tokenizers::tokenize_sentences(dt$text_clean)

    tokenized_dt <- data.table::rbindlist(
      lapply(seq_along(tokenized_list), function(i) {
        sents <- unlist(tokenized_list[[i]])

        # further split long sentences
        sents_split <- unlist(lapply(seq_along(sents), function(j) {
          if (tokenizers::count_words(sents[j]) > max_words) {
            tokenizers::chunk_text(sents[j],
                                   doc_id = paste0(i, "_", j),
                                   chunk_size = max_words)
          } else {
            sents[j]
          }
        }))

        data.table::data.table(
          doc_idx = dt$doc_idx[i],
          sen_idx = seq_along(sents_split),
          text_clean = sents_split
        )
      }),
      use.names = TRUE, fill = TRUE
    )

    # --- safer join of metadata (no recycling issues) ---
    meta_cols <- setdiff(names(dt), "text_clean")
    tokenized_dt <- dt[, ..meta_cols][tokenized_dt, on = "doc_idx"]

    dt <- tokenized_dt
  } else {
    dt[, sen_idx := 1L]
  }

  # --- Create sen_id ---
  dt[, sen_id := paste0(doc_idx, "_", sen_idx)]

  # --- Column order ---
  cols_order <- c("sen_id", "doc_idx", "sen_idx", "id", "text_orig", "text_clean", "lang_guess")
  cols_exist <- intersect(cols_order, names(dt))
  dt <- dt[, ..cols_exist]
  data.table::setcolorder(dt, cols_exist)

  # --- Sorting ---
  data.table::setorder(dt, doc_idx, sen_idx)

  if (return_string) {
    return(dt$text_clean)
  } else {
    return(dt[])
  }
}


#' @title Detect Languages with fastText
#'
#' @description Identifies the language of each text in a character vector or data.frame
#' using fastText's pretrained language identification model. Calls `clean_text()`
#' internally to normalize and optionally tokenize the text before prediction.
#'
#' @inheritParams clean_text
#' @param model_path Character, path to the fastText `lid.176.bin` model.
#' Defaults to `~/.cache/easieRnmt/lid.176.bin`.
#' @param prob_threshold Numeric, threshold below which the detected language is replaced
#' by `und_label` (or by the user-provided `lang_guess`). Default = 0.25.
#' @param und_label Character, label for undefined language if confidence is below
#' `prob_threshold` and no `lang_guess` column is provided. Default = "und".
#' @param threads Integer, number of threads for fastText. Default = `parallel::detectCores()`.
#' @param ... Additional parameters passed on to `clean_text()`.
#'
#' @return A data.table with standardized columns:
#' `sen_id`, `doc_idx`, `sen_idx`, optional `id`, `text_orig`,
#' `text_clean`, optional `lang_guess`, `lang`, and `lang_prob`.
#'
#' @import data.table
#' @export
detect_languages <- function(x,
                             text_col = "text",
                             id_col = NULL,
                             lang_guess_col = NULL,
                             model_path = NULL,
                             prob_threshold = 0.25,
                             und_label = "und",
                             max_char = 5000,
                             tokenize_sentences = FALSE,
                             threads = parallel::detectCores(),
                             verbose = TRUE,
                             max_words = 50,
                             ...) {
  if (!requireNamespace("fastText", quietly = TRUE)) stop("Package 'fastText' must be installed.")
  vmessage <- function(...) if (verbose) message(...)

  # Step 1: Clean (and optionally tokenize) input
  dt <- clean_text(
    x = x,
    text_col = text_col,
    id_col = id_col,
    lang_guess_col = lang_guess_col,
    max_char = max_char,
    tokenize_sentences = tokenize_sentences,
    verbose = verbose,
    max_words = max_words,
    ...
  )

  # Step 2: ensure fastText model is available
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

  # Step 3: run fastText
  empty_idx <- which(dt$text_clean == "")
  texts_for_ft <- dt$text_clean
  texts_for_ft[empty_idx] <- " "

  vmessage("Running fastText language detection...")
  ft_res <- tryCatch(
    fastText::language_identification(
      texts_for_ft,
      pre_trained_language_model_path = model_path,
      threads = threads
    ),
    error = function(e) stop("fastText failed: ", conditionMessage(e))
  )

  if (nrow(ft_res) != nrow(dt)) {
    stop("fastText returned ", nrow(ft_res), " rows for ", nrow(dt), " inputs.")
  }

  # Step 4: attach predictions
  dt[, `:=`(
    lang = ft_res$iso_lang_1,
    lang_prob = ft_res$prob_1
  )]

  if (length(empty_idx) > 0) {
    dt[empty_idx, `:=`(lang = und_label, lang_prob = 0)]
  }
  dt[lang_prob < prob_threshold, lang := und_label]

  if ("lang_guess" %in% names(dt)) {
    dt[lang == und_label & !is.na(lang_guess) & lang_guess != "", lang := lang_guess]
  }

  # Step 5: column order
  cols_order <- c("sen_id", "doc_idx", "sen_idx", "id", "text_orig",
                  "text_clean", "lang_guess", "lang", "lang_prob")
  cols_exist <- intersect(cols_order, names(dt))
  dt <- dt[, ..cols_exist]

  return(dt[])
}


#' @title Preprocess Text for Translation
#'
#' @description Wrapper around `detect_languages()` that detects languages,
#' reprocesses low-confidence cases, and splits the data into homogeneous
#' language groups for translation. Tokenization can be performed before or
#' after language detection.
#'
#' @inheritParams detect_languages
#' @param targ_lang Character, fallback language code when detection is uncertain.
#' @param chunk_size Integer, optional. If provided, split each homogeneous
#' language data.table into chunks of at most `chunk_size` rows.
#' @param tokenize_when Character, determines when to tokenize: `"before"` or `"after"`.
#'
#' @return A named list of data.tables, each containing texts of one homogeneous
#' language or language+part chunk.
#' @import data.table
#' @export
preprocess <- function(x,
                       text_col = "text",
                       id_col = NULL,
                       lang_guess_col = NULL,
                       targ_lang,
                       model_path = NULL,
                       prob_threshold = 0.25,
                       und_label = "und",
                       max_char = 5000,
                       chunk_size = NULL,
                       tokenize_sentences = TRUE,
                       tokenize_when = "after",
                       threads = parallel::detectCores(),
                       verbose = TRUE,
                       max_words = max_words,
                       ...) {
  if (missing(targ_lang)) stop("'targ_lang' must be specified")
  vmessage <- function(...) if (verbose) message(...)

  # Decide when to tokenize
  tokenize_before <- (tokenize_sentences && tokenize_when == "before")
  tokenize_after  <- (tokenize_sentences && tokenize_when == "after")

  # Step 1: run language detection
  dt <- detect_languages(
    x = x,
    text_col = text_col,
    id_col = id_col,
    lang_guess_col = lang_guess_col,
    model_path = model_path,
    prob_threshold = prob_threshold,
    und_label = und_label,
    max_char = max_char,
    threads = threads,
    verbose = verbose,
    tokenize_sentences = tokenize_before,
    max_words = max_words,
    ...
  )

  # Step 2: handle uncertain cases
  if(!is.null(lang_guess_col) && und_label != targ_lang) {

    idx_und <- which(dt$lang == und_label)
    if (length(idx_und) > 0) {
      vmessage("Re-cleaning ", length(idx_und), " uncertain texts...")

      length(dt$text_orig[idx_und])
      length(dt$text_clean[idx_und])

      dt$text_clean[idx_und] <- clean_text(
        x = dt$text_orig[idx_und],
        replace_alphaless = FALSE,
        return_string = TRUE,
        verbose = verbose,
        tokenize_sentences = F
      )

      if ("lang_guess" %in% names(dt)) {
        dt[idx_und, lang := data.table::fifelse(!is.na(lang_guess) & lang_guess != "",
                                                lang_guess, targ_lang)]
      } else {
        dt[idx_und, lang := targ_lang]
      }
    }
  }


  # Step 3: tokenize after language detection if requested
  if (tokenize_after) {
    vmessage("Tokenizing texts after language detection...")

    # Copy and clean
    x <- data.table::copy(dt)

    if("sen_id" %in% names(x)) x[,sen_id := NULL]
    if("sen_idx" %in% names(x)) x[,sen_idx := NULL]


    # Save predictions first
    lang_meta <- x[, .(doc_idx, lang, lang_prob)]

    # Re-run clean_text with tokenization
    dt <- clean_text(
      x = x,
      text_col = "text_clean",
      id_col = "id",
      lang_guess_col = "lang_guess",
      tokenize_sentences = TRUE,
      verbose = verbose
    )

    # Re-attach predictions
    dt <- merge(dt, lang_meta, by = "doc_idx", all.x = TRUE)
  }

  # Step 4: split into homogeneous groups
  out <- split(dt, by = "lang", keep.by = TRUE, sorted = TRUE)

  # Step 5: optional chunking
  if (!is.null(chunk_size)) {
    chunked_out <- list()
    for (lang_name in names(out)) {
      dt_lang <- out[[lang_name]]
      n <- nrow(dt_lang)
      if (n > chunk_size) {
        idx_split <- split(seq_len(n), ceiling(seq_along(seq_len(n)) / chunk_size))
        for (i in seq_along(idx_split)) {
          chunk <- dt_lang[idx_split[[i]], ]
          chunk_name <- paste0(lang_name, "_pt", i)
          chunked_out[[chunk_name]] <- chunk
        }
      } else {
        chunked_out[[lang_name]] <- dt_lang
      }
    }
    out <- chunked_out
  }

  return(out)
}

