#' @title Preprocess and Translate Texts
#'
#' @description Detects languages, reprocesses uncertain cases, splits the input
#'   into homogeneous language groups, and calls the Python translator
#'   (\code{easynmt_translate()}) on each group.
#'
#' @inheritParams preprocess
#' @param target_lang Character, target language for translation.
#' @param model Character, translation model (default "opus-mt").
#' @param seed Integer, random seed (default 42).
#' @param batch_size Integer, batch size for translation (default 20L).
#' @param max_length_tl Integer, maximum token length (default 512L).
#' @param beam_size Integer, beam size (default 1L).
#' @param deterministic Logical, enforce deterministic cudnn ops (default TRUE).
#' @param check_translation Logical, perform retry check (default FALSE).
#' @param n_retries Integer, number of retries if check fails (default 3L).
#' @param div_threshold Numeric, threshold for retry trigger (default 0.5).
#' @param return_string Logical, if TRUE, return only the translated character vector. Default = FALSE.
#' @param save_dir Optional character path. If provided, saves each processed
#'   subset as an \code{.rds} file with its language/part name.
#' @param ... Additional parameters passed on to \code{clean_text()} during preprocessing.
#'
#' @return A data.table with original data plus translation results merged in.
#' @export
translate <- function(
    x,
    text_col = "text",
    id_col = NULL,
    lang_guess_col = NULL,
    targ_lang,
    model_path = NULL,
    prob_threshold = 0.25,
    und_label = "und",
    max_char = 5000,
    threads = parallel::detectCores(),
    target_lang,
    model = "opus-mt",
    seed = 42L,
    batch_size = 20L,
    max_length_tl = 512L,
    beam_size = 1L,
    deterministic = TRUE,
    verbose = TRUE,
    check_translation = FALSE,
    n_retries = 3L,
    div_threshold = 0.5,
    return_string = FALSE,
    save_dir = NULL,
    ...
) {
  vmessage <- function(...) if (verbose) message(...)

  # --- sanity check: environment initialized? ---
  if (is.null(options("easynmt_initialized")$easynmt_initialized)) {
    stop("EasyNMT environment not initialized. Please run initialize_easynmt() first.")
  }

  # --- Step 1: check if x is already preprocessed ---
  if (is.list(x) && all(c("row_id", "lang", "text_clean") %in% names(x[[1]]))) {
    preproc_list <- x
    vmessage("Input detected as preprocessed. Skipping preprocess().")
  } else {
    preproc_list <- preprocess(
      x = x,
      text_col = text_col,
      id_col = id_col,
      lang_guess_col = lang_guess_col,
      targ_lang = targ_lang,
      model_path = model_path,
      prob_threshold = prob_threshold,
      und_label = und_label,
      max_char = max_char,
      threads = threads,
      verbose = verbose,
      ...
    )
  }

  # --- Helper for one subset ---
  process_one <- function(dt, lang_name) {
    if (verbose) message("Processing language: ", lang_name)

    res <- reticulate::py$easynmt_translate(
      df = dt,
      target_lang = targ_lang,
      model = model,
      seed = as.integer(seed),
      deterministic = deterministic,
      max_length_tl = as.integer(max_length_tl),
      batch_size = as.integer(batch_size),
      beam_size = as.integer(beam_size),
      verbose = verbose,
      check_translation = check_translation,
      n_retries = as.integer(n_retries),
      div_threshold = div_threshold
    )

    res <- data.table::as.data.table(res)

    out <- merge(
      dt,
      res[, .(
        row_id,
        lang_target,
        translation,
        tl_error = error,
        tl_datetime,
        tl_model
      )],
      by = "row_id", all.x = TRUE
    )

    # Sanity check: clear translations if text_orig was NA
    if ("text_orig" %in% names(out)) {
      out[is.na(text_orig), translation := NA_character_]
    }

    # Optional saving
    if (!is.null(save_dir)) {
      if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
      file_path <- file.path(save_dir, paste0(lang_name, ".rds"))
      saveRDS(out, file_path)
      vmessage("Saved translated chunk to: ", file_path)
    }

    return(out[])
  }

  # --- Step 2: Apply to all language groups ---
  out_list <- pbapply::pblapply(
    X = seq_along(preproc_list),
    FUN = function(i) process_one(preproc_list[[i]], names(preproc_list)[i]),
    cl = NULL
  )

  # --- Step 3: Bind results ---
  out <- data.table::rbindlist(out_list, use.names = TRUE, fill = TRUE)
  data.table::setorder(out, row_id)

  if (return_string) {
    return(out$translation)
  } else {
    return(out[])
  }
}
