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


#' @title Clean and Normalize Text for Language Detection and Translation
#'
#' @description Cleans and normalizes raw text data in preparation for language
#' detection and machine translation. Handles emoji replacement, removal of
#' unsupported characters, trimming, squishing whitespace, and limiting text
#' length. Returns either a standardized data.table or a character vector.
#'
#' @param x Character vector or data.frame containing texts to clean.
#' @param text_col Character, name of the text column if \code{x} is a data.frame.
#' Ignored if \code{x} is a character vector. Default = "text".
#' @param id_col Character, optional column name in the input data.frame to
#' preserve as an identifier. Default = NULL.
#' @param lang_guess_col Character, optional column name in the input data.frame
#' to preserve as a column named \code{lang_guess}. Default = NULL.
#' @param replace_emojis Logical, whether to replace emojis with placeholder names.
#' Default = TRUE.
#' @param replace_alphaless Logical, whether to replace strings that contain no
#' alphabetic characters with empty strings. Default = TRUE.
#' @param max_char Integer, maximum number of characters per text. Texts longer
#' than this are truncated. Default = 5000.
#' @param return_string Logical, if TRUE, return only the cleaned character vector
#' (\code{text_clean}) instead of a full data.table. Default = FALSE.
#' @param verbose Logical, whether to print progress messages. Default = TRUE.
#'
#' @details
#' The cleaning pipeline performs the following steps:
#' \enumerate{
#'   \item Convert input to UTF-8.
#'   \item Replace \code{NA} with empty strings.
#'   \item Optionally replace emojis with placeholder names if
#'   \code{replace_emojis = TRUE}.
#'   \item Remove all characters except letters, numbers, punctuation, and
#'   whitespace.
#'   \item Normalize encoding to UTF-8 and substitute invalid bytes.
#'   \item Truncate texts longer than \code{max_char}.
#'   \item Optionally remove texts without alphabetic content if
#'   \code{replace_alphaless = TRUE}.
#'   \item Collapse multiple spaces into a single space.
#' }
#'
#' The returned data.table is restricted to the columns
#' \code{row_id}, \code{id} (if present), \code{text_orig}, \code{text_clean},
#' and \code{lang_guess} (if present), in this fixed order.
#'
#' @return Either:
#' \itemize{
#'   \item A data.table with columns:
#'   \itemize{
#'     \item \code{row_id}: Row index of the input.
#'     \item \code{id}: User-provided identifier, if available.
#'     \item \code{text_orig}: Original text before cleaning.
#'     \item \code{text_clean}: Cleaned and normalized text.
#'     \item \code{lang_guess}: Optional language guess column, if present.
#'   }
#'   \item Or a character vector of cleaned texts if \code{return_string = TRUE}.
#' }
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
                       return_string = FALSE,
                       verbose = TRUE) {

  vmessage <- function(...) if (verbose) message(...)

  if (is.character(x)) {
    dt <- data.table::data.table(row_id = seq_along(x), text_orig = x)
  } else if (is.data.frame(x)) {
    if (!text_col %in% names(x)) stop("text_col not found in data.frame")
    dt <- data.table::as.data.table(x)
    dt[, row_id := .I]
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

  dt[, text_clean := enc2utf8(text_orig)]
  dt[is.na(text_clean), text_clean := ""]
  if(replace_emojis)  dt[, text_clean := replace_emoji_with_name(text_clean)]

  # keep only letters, numbers, punctuation, whitespace
  dt[, text_clean := stringi::stri_replace_all_regex(text_clean, "[^\\p{L}\\p{N}\\p{P}\\p{Zs}]", " ")]


  dt[, text_clean := iconv(text_clean, from = "", to = "UTF-8", sub = " ")]
  dt[nchar(text_clean) > max_char, text_clean := substr(text_clean, 1, max_char)]
  if(replace_alphaless) dt[!grepl("[[:alpha:]]", text_clean), text_clean := ""]
  dt[, text_clean := textclean::replace_curly_quote(text_clean)]
  dt[, text_clean := stringr::str_squish(text_clean)]

  # Ensure column order: row_id, id?, text_orig, text_clean, lang_guess?
  cols_order <- c("row_id", "id", "text_orig", "text_clean", "lang_guess")
  cols_exist <- intersect(cols_order, names(dt))
  dt <- dt[, ..cols_exist]
  data.table::setcolorder(dt, cols_exist)

  if(return_string){
    return(dt$text_clean)
  }else{
    return(dt[])
  }


}



#' @title Detect Languages with fastText
#'
#' @description Identifies the language of each text in a character vector or data.frame
#' using fastText's pretrained language identification model. Calls \code{clean_text()}
#' internally to normalize the text before prediction. Returns a standardized data.table.
#'
#' @param x Character vector or data.frame containing texts to identify.
#' @param text_col Character, name of the text column if \code{x} is a data.frame.
#' Ignored if \code{x} is a vector. Default = "text".
#' @param id_col Character, optional column name in the input data.frame to preserve
#' as an identifier. Default = NULL.
#' @param lang_guess_col Character, optional column in the input data.frame containing
#' user-specified language guesses. If present, these are used instead of \code{und_label}.
#' Default = NULL.
#' @param model_path Character, path to the fastText \code{lid.176.bin} model.
#' Defaults to \code{~/.cache/easieRnmt/lid.176.bin}.
#' @param prob_threshold Numeric, threshold below which the detected language is replaced
#' by "und" (or by the user-provided \code{lang_guess}). Default = 0.25.
#' @param und_label Character, label for undefined language if confidence is below
#' \code{prob_threshold} and no \code{lang_guess} column is provided. Default = "und".
#' @param max_char Integer, maximum number of characters per text after cleaning.
#' Texts longer than this are truncated. Default = 5000.
#' @param threads Integer, number of threads for fastText. Default = parallel::detectCores().
#' @param verbose Logical, whether to print progress messages. Default = TRUE.
#' @param ... Additional parameters passed on to \code{clean_text}.
#'
#' @return A data.table with standardized columns: \code{row_id}, optional \code{id},
#' \code{text_orig}, \code{text_clean}, \code{lang_guess}.
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
                             threads = parallel::detectCores(),
                             verbose = TRUE,
                             ...) {
  if (!requireNamespace("fastText", quietly = TRUE)) stop("Package 'fastText' must be installed.")
  vmessage <- function(...) if (verbose) message(...)

  # Clean and standardize input
  dt <- clean_text(
    x = x,
    text_col = text_col,
    id_col = id_col,
    lang_guess_col = lang_guess_col,
    max_char = max_char,
    verbose = verbose,
    ...
  )

  # Resolve fastText model path
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

  # Prepare input for fastText
  empty_idx <- which(dt$text_clean == "")
  texts_for_ft <- dt$text_clean
  texts_for_ft[empty_idx] <- " "

  # Run fastText
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
    stop("fastText returned ", nrow(ft_res), " rows for ", nrow(dt),
         " inputs. Likely cause: unhandled special characters.")
  }

  # Attach predictions
  dt[, `:=`(
    lang = ft_res$iso_lang_1,
    lang_prob = ft_res$prob_1
  )]

  # Low-confidence handling
  if (length(empty_idx) > 0) {
    dt[empty_idx, `:=`(lang = und_label, lang_prob = 0)]
  }
  dt[lang_prob < prob_threshold, lang := und_label]

  # If user provided a lang_guess column, prefer it over und_label
  if ("lang_guess" %in% names(dt)) {
    dt[lang == und_label & !is.na(lang_guess) & lang_guess != "", lang := lang_guess]
  }

  # Ensure standardized schema, including detection results
  cols_order <- c("row_id", "id", "text_orig", "text_clean", "lang_guess", "lang", "lang_prob")
  cols_exist <- intersect(cols_order, names(dt))
  dt <- dt[, ..cols_exist]
  data.table::setcolorder(dt, cols_exist)

  return(dt[])
}



#' @title Preprocess Text for Translation
#'
#' @description Wrapper around \code{detect_languages()} that detects languages,
#' reprocesses low-confidence cases, and splits the data into homogeneous
#' language groups for translation. Optionally splits large groups into
#' smaller chunks.
#'
#' @param x Character vector or data.frame containing texts to process.
#' @param text_col Character, name of the text column if \code{x} is a data.frame.
#' Ignored if \code{x} is a character vector. Default = "text".
#' @param id_col Character, optional column name in the input data.frame to
#' preserve as an identifier. Default = NULL.
#' @param lang_guess_col Character, optional column name in the input data.frame
#' to preserve as a column named \code{lang_guess}. Default = NULL.
#' @param targ_lang Character, fallback language code to assign when fastText
#' detection is uncertain and no \code{lang_guess} is available. Required.
#' @param model_path Character, path to the fastText \code{lid.176.bin} model.
#' Defaults to \code{~/.cache/easieRnmt/lid.176.bin}.
#' @param prob_threshold Numeric, threshold below which the detected language is
#' replaced by \code{und_label}. Default = 0.25.
#' @param und_label Character, label for undefined language. Default = "und".
#' @param max_char Integer, maximum number of characters per text after cleaning.
#' Texts longer than this are truncated. Default = 5000.
#' @param chunk_size Integer, optional. If provided, split each homogeneous
#' language data.table into chunks of at most \code{chunk_size} rows.
#' Chunked tables are suffixed with \code{"_pt1"}, \code{"_pt2"}, etc.
#' Default = NULL (no further splitting).
#' @param tokenize_sentences Logical, whether to split texts with more than
#' 3 sentences into individual sentences. Default = FALSE.
#' @param threads Integer, number of threads for fastText. Default =
#' \code{parallel::detectCores()}.
#' @param verbose Logical, whether to print progress messages. Default = TRUE.
#' @param ... Additional parameters passed on to \code{clean_text()}.
#'
#' @return A named list of data.tables, each containing texts of one homogeneous
#' language or language+part chunk. The list names correspond to the language
#' codes (or suffixed with \code{"_pt"} for chunked groups).
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
                       tokenize_sentences = FALSE,
                       threads = parallel::detectCores(),
                       verbose = TRUE,
                       ...) {
  if (missing(targ_lang)) stop("'targ_lang' must be specified")

  vmessage <- function(...) if (verbose) message(...)

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
    ...
  )

  # Step 2: handle uncertain cases
  idx_und <- which(dt$lang == und_label)

  if (length(idx_und) > 0) {
    vmessage("Re-cleaning ", length(idx_und), " uncertain texts...")

    # Re-clean text_orig for these cases
    dt$text_clean[idx_und] <- clean_text(
      x = dt$text_orig[idx_und],
      replace_alphaless = FALSE,
      return_string = TRUE,
      verbose = verbose
    )

    # Replace lang with lang_guess (if available) or targ_lang
    if ("lang_guess" %in% names(dt)) {
      dt[idx_und, lang := data.table::fifelse(!is.na(lang_guess) & lang_guess != "",
                                              lang_guess,
                                              targ_lang)]
    } else {
      dt[idx_und, lang := targ_lang]
    }
  }

  # Step 2b: sentence tokenization
  if (tokenize_sentences) {
    vmessage("Tokenizing long texts into sentences...")

    # Count sentences
    dt[, n_sen := tokenizers::count_sentences(text_clean)]

    # Split rows with >3 sentences
    dt_long <- dt[n_sen > 3]
    dt_short <- dt[n_sen <= 3]

    if (nrow(dt_long) > 0) {
      tokenized_list <- tokenizers::tokenize_sentences(dt_long$text_clean)

      # Expand into data.table
      tokenized_dt <- data.table::rbindlist(
        lapply(seq_along(tokenized_list), function(i) {
          sents <- unlist(tokenized_list[[i]])
          data.table::data.table(
            row_id = paste0(dt_long$row_id[i], "_", seq_along(sents)),
            orig_row_id = dt_long$row_id[i],
            text_clean = sents
          )
        }),
        use.names = TRUE, fill = TRUE
      )

      # Merge metadata back (everything except text_clean/n_sen)
      meta_cols <- setdiff(names(dt_long), c("text_clean", "n_sen"))
      tokenized_dt <- merge(
        tokenized_dt,
        dt_long[, ..meta_cols],
        by.x = "orig_row_id", by.y = "row_id",
        all.x = TRUE
      )

      # Drop helper
      tokenized_dt[, orig_row_id := NULL]
      data.table::setnames(tokenized_dt, "row_id", "sen_row_id")
      data.table::setnames(tokenized_dt, "sen_row_id", "row_id")

      # Combine back
      dt <- data.table::rbindlist(list(dt_short, tokenized_dt), use.names = TRUE, fill = TRUE)
    }

    dt[, n_sen := NULL] # cleanup
  }

  # Step 3: split into homogeneous groups
  out <- split(dt, by = "lang", keep.by = TRUE, sorted = TRUE)

  # Step 4: optional chunking
  if (!is.null(chunk_size)) {
    chunked_out <- list()
    for (lang_name in names(out)) {
      dt_lang <- out[[lang_name]]
      n <- nrow(dt_lang)
      if (n > chunk_size) {
        nchunks <- ceiling(n / chunk_size)
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
