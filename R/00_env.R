#' @title Initialize the easynmt Python environment
#'
#' @description
#' Declares all required Python packages via \code{\link[reticulate]{py_require}},
#' which provisions them using uv into an ephemeral virtual environment.
#' PyTorch is installed with \code{UV_TORCH_BACKEND=auto}, which lets uv
#' auto-detect NVIDIA (CUDA), AMD (ROCm), and Intel GPUs and pick the correct
#' wheels — no manual driver querying needed. Pass \code{gpu = FALSE} to force
#' CPU-only onnxruntime.
#'
#' The function is called automatically at the start of each easynmt function
#' and is a no-op after the first successful call within a session.
#'
#' @details
#' Non-torch packages are declared with \code{py_require()} so that reticulate /
#' uv can resolve them together. PyTorch is handled by setting
#' \code{UV_TORCH_BACKEND=auto}, which instructs uv to query for CUDA, ROCm,
#' and Intel GPU drivers and select the matching PyTorch index automatically.
#'
#' \code{onnxruntime-gpu} only supports CUDA, so the \code{gpu} parameter
#' reflects NVIDIA presence only; AMD and Intel users get CPU onnxruntime while
#' still getting GPU-accelerated PyTorch.
#'
#' \strong{fasttext workaround:} EasyNMT declares \code{fasttext} as a hard
#' \code{Requires-Dist} dependency (not optional). The canonical \code{fasttext}
#' package has no prebuilt wheels for Windows and fails to compile from C++
#' source on MSVC. The workaround uses \code{UV_OVERRIDE} with a direct-URL
#' specifier that points \code{fasttext} at the \code{fasttext-predict} wheel —
#' a prediction-only fork that ships prebuilt binary wheels for all platforms
#' and Python 3.9–3.13, and exposes the same \code{import fasttext} namespace.
#' The override file contains \code{fasttext @ https://...} with the PyPI URL
#' for the matching wheel, selected at runtime by platform and Python version.
#' Language detection is handled in R, so the prediction-only scope of
#' \code{fasttext-predict} is not a limitation.
#'
#' @param gpu Logical. Whether to install \code{onnxruntime-gpu} (CUDA only).
#'   Defaults to auto-detection via \code{check_gpu()}.
#' @param uv_cache_dir Character (optional). Directory used by uv to install python libraries.
#' @param models_dir Character (optional).  Directory used to cache huggingface models.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
initialize_easynmt <- function(gpu = check_gpu(),
                                  uv_cache_dir = NULL,
                                  models_dir = NULL) {
  if (isTRUE(.env$initialized_easynmt)) {
    return(invisible(TRUE))
  }

  # Set custom paths if provided
  if (!is.null(uv_cache_dir)) {
    Sys.setenv(UV_CACHE_DIR = uv_cache_dir)
  }

  if (!is.null(models_dir)) {
    Sys.setenv(TORCH_HOME = models_dir)
    Sys.setenv(HF_HOME = models_dir)
    Sys.setenv(TRANSFORMERS_CACHE = file.path(models_dir, "transformers"))
  }

  # Set torch backend FIRST, before any Python/torch initialization
  Sys.setenv(UV_TORCH_BACKEND = "auto")
  if (isTRUE(gpu)) {
    Sys.setenv(UV_TORCH_DEVICE = "cuda")
  }

  # fasttext override -------------------------------------------------------
  # Only needed on Windows — Linux and macOS can build fasttext from source or
  # use prebuilt manylinux/macos wheels without issue.
  if (.Platform$OS.type == "windows") {
    Sys.setenv(UV_OVERRIDE = .fasttext_override_file())
  }

  # Non-torch NLP stack — resolved together by uv via py_require
  reticulate::py_require(
    c(
      "numpy",
      "pandas",
      "pytz",
      "python-dateutil",
      "nltk",
      "protobuf",
      "sentencepiece==0.2.0",
      "tqdm",
      "langdetect",
      "sacremoses",
      "easynmt"
    )
  )

  # Torch stack with environment variables already set
  reticulate::py_require(c("torch", "torchvision", "torchaudio"))

  if (isTRUE(gpu)) {
    reticulate::py_require("onnxruntime-gpu")
  } else {
    reticulate::py_require("onnxruntime")
  }

  # Load python function
  reticulate::import("easynmt")
  reticulate::source_python(
    system.file("python", "easynmt_translate.py", package = "easieRnmt")
  )

  .env$initialized_easynmt <- TRUE
  invisible(TRUE)
}


#' Build a UV_OVERRIDE file that redirects fasttext -> fasttext-predict
#'
#' Selects the correct fasttext-predict wheel URL for the current platform and
#' Python version, writes it to a per-session temp file, and returns the path.
#' uv reads this file via UV_OVERRIDE and satisfies any `fasttext` requirement
#' with the specified wheel instead of attempting a source build.
#'
#' @return Path to the override requirements file (character).
#' @keywords internal
#' @noRd
.fasttext_override_file <- function() {
  # fasttext-predict 0.9.2.4 wheel URLs keyed by Python minor version (Windows only).
  wheels <- list(
    "39"  = "https://files.pythonhosted.org/packages/fa/da/2ab0060f805449bfe22dafbd441d5b94b2d9f4ab185dd2c0436bd1db56ea/fasttext_predict-0.9.2.4-cp39-cp39-win_amd64.whl",
    "310" = "https://files.pythonhosted.org/packages/6d/33/df75b2a1e207eda91efe35766e09dba41ef735e390b156c9c3adc0014e68/fasttext_predict-0.9.2.4-cp310-cp310-win_amd64.whl",
    "311" = "https://files.pythonhosted.org/packages/34/b0/456578e7269dace3d7a80a34b30c7757aea6aa34601853c58e5ad186d3d6/fasttext_predict-0.9.2.4-cp311-cp311-win_amd64.whl",
    "312" = "https://files.pythonhosted.org/packages/a7/c5/11c1f50b47f492d562974878ec34b6a0b84699f8b05e1cc3a75c65349784/fasttext_predict-0.9.2.4-cp312-cp312-win_amd64.whl",
    "313" = "https://files.pythonhosted.org/packages/9c/1d/c1ccc8790ce54200c84164d99282f088dddb9760aeefc8860856aafa40b4/fasttext_predict-0.9.2.4-cp313-cp313-win_amd64.whl"
  )

  # Detect Python minor version from the uv-managed Python directory on disk.
  # Deliberately avoids reticulate::py_config() -- calling it here would
  # initialize Python before py_require() has provisioned the uv environment,
  # causing all subsequent py_require() calls to fail with "only action='add'
  # is supported".
  py_minor <- tryCatch({
    uv_py_dir <- file.path(Sys.getenv("APPDATA"), "uv", "python")
    candidates <- list.dirs(uv_py_dir, recursive = FALSE, full.names = FALSE)
    cpython <- sort(grep("^cpython", candidates, value = TRUE), decreasing = TRUE)[1]
    # e.g. "cpython-3.12.13-windows-x86_64-none" -> "12"
    ver_parts <- strsplit(cpython, "-")[[1]][2]   # "3.12.13"
    strsplit(ver_parts, "\\.")[[1]][2]            # "12"
  }, error = function(e) "12")  # default to 3.12

  url <- wheels[[paste0("3", py_minor)]]

  if (is.null(url)) {
    warning(
      "No fasttext-predict wheel URL found for Python 3.", py_minor,
      " on Windows. fasttext may fail to install.",
      call. = FALSE
    )
    return(NULL)
  }

  override_file <- file.path(tempdir(), "uv_overrides_easynmt.txt")
  writeLines(paste0("fasttext @ ", url), override_file)
  override_file
}


#' CUDA-detection only, just for onnxruntime-gpu
#' @keywords internal
#' @noRd
check_gpu <- function() {
  if (!is.null(.env$gpu_info)) {
    return(.env$gpu_info$n > 0)
  }
  has_nvidia <- nzchar(Sys.which("nvidia-smi")) || file.exists("/dev/nvidiactl")
  .env$gpu_info <- list(n = as.integer(has_nvidia))
  has_nvidia
}


#' Check easynmt Backend Configuration
#'
#' Displays diagnostic information about the easynmt Python backend setup.
#'
#' @return Invisibly returns a list with backend configuration details.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   easynmt::check_backend()
#' }
check_backend <- function() {
  gpu_available <- check_gpu()

  if (!reticulate::py_available()) {
    cli::cli_alert_warning("Python not initialized. Call initialize_easynmt() first.")
    return(invisible(NULL))
  }

  torch <- reticulate::import("torch")
  torch_version <- torch$`__version__`

  home <- path.expand("~")
  uv_cache <- file.path(Sys.getenv("LOCALAPPDATA"), "uv")
  hf_cache <- file.path(home, ".cache", "huggingface")

  cli::cli_h2("easynmt Backend")
  cli::cli_ul(c(
    "GPU detected: {if(gpu_available) 'YES' else 'NO'}",
    "PyTorch: {torch_version}",
    "UV cache: {uv_cache}",
    "Models cache: {hf_cache}"
  ))

  invisible(list(gpu = gpu_available, torch_version = torch_version))
}