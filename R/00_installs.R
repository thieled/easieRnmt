#' @title Install and Set Up a Virtual Environment
#'
#' @description Ensures Miniconda is installed, provisions the requested Python
#' version into it, creates a virtualenv with that interpreter, activates it,
#' and removes the temporary conda env.
#'
#' @param python_version Character. Python version for the virtualenv. Default "3.11".
#' @param venv_name Character. Virtualenv name. Default "r-easynmt".
#' @param conda_path Character. Path to Miniconda. If NULL, autodetect.
#' @param ask Logical. If TRUE, prompt before installing Miniconda. Default TRUE.
#' @param force Logical. If TRUE, force (re)install the venv. Default FALSE.
#' @param verbose Logical. If TRUE, print progress messages. Default TRUE.
#'
#' @return Invisibly returns NULL.
#' @export
install_conda_venv <- function(python_version = "3.11",
                               venv_name = "r-easynmt",
                               conda_path = NULL,
                               ask = TRUE,
                               force = FALSE,
                               verbose = TRUE) {
  vmessage <- function(...) if (verbose) message(...)

  # Step 1: Ensure Miniconda is installed
  t <- tryCatch({
    if (!is.null(conda_path)) {
      reticulate::conda_list(conda = file.path(conda_path, "Scripts", "conda.exe"))
    } else {
      reticulate::conda_list()
    }
  }, error = function(e) NULL)

  if (is.null(t)) {
    permission <- TRUE
    if (ask) {
      cli::cli_alert_warning("No suitable conda installation was found.")
      response <- tolower(trimws(readline("Install Miniconda? (y/n): ")))
      permission <- response %in% c("y", "yes")
    }
    if (!permission) stop("Aborted by user")

    if (!is.null(conda_path)) {
      vmessage("Installing Miniconda at: ", conda_path)
      reticulate::install_miniconda(path = conda_path, force = force)
    } else {
      vmessage("Installing Miniconda at default location.")
      reticulate::install_miniconda(force = force)
    }
    t <- if (!is.null(conda_path)) {
      reticulate::conda_list(conda = file.path(conda_path, "Scripts", "conda.exe"))
    } else {
      reticulate::conda_list()
    }
  }

  # Step 2: Define conda binary from chosen installation
  conda_bin <- if (!is.null(conda_path)) {
    if (Sys.info()[["sysname"]] == "Windows") {
      normalizePath(file.path(conda_path, "Scripts", "conda.exe"))
    } else {
      normalizePath(file.path(conda_path, "bin", "conda"))
    }
  } else {
    "conda"
  }

  # Step 3: Ensure requested Python version via tmp env
  tmp_env <- paste0("tmp-", gsub("\\.", "", python_version))
  if (!tmp_env %in% t$name) {
    vmessage("Creating temporary conda env with Python ", python_version,
             " using ", conda_bin)
    system2(conda_bin, c("create", "-y", "-n", tmp_env, paste0("python=", python_version)))
    t <- reticulate::conda_list(conda = conda_bin)
  }
  py_bin <- t$python[t$name == tmp_env]

  # Step 4: Create the venv with that binary
  env_exists <- reticulate::virtualenv_exists(envname = venv_name)
  if (!env_exists || force) {
    vmessage("Creating virtualenv: ", venv_name, " with Python: ", py_bin)
    reticulate::virtualenv_create(envname = venv_name, python = py_bin, force = TRUE)
  } else {
    vmessage("Virtualenv already exists: ", venv_name)
  }

  # Step 5: Activate the venv
  vmessage("Activating virtual environment: ", venv_name)
  reticulate::use_virtualenv(venv_name, required = TRUE)

  invisible(NULL)
}
