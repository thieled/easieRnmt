#' @title Install and Set Up a Virtual Environment
#'
#' @description Ensures Miniconda is installed, provisions the requested Python
#' version into it if necessary, creates a virtualenv with that interpreter,
#' and activates it.
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
  if (!is.null(conda_path)) {
    conda_bin <- if (Sys.info()[["sysname"]] == "Windows") {
      normalizePath(file.path(conda_path, "Scripts", "conda.exe"))
    } else {
      normalizePath(file.path(conda_path, "bin", "conda"))
    }
  } else {
    t <- reticulate::conda_list()
    base_python <- t$python[t$name %in% c("base", "root")][1]
    if (is.na(base_python)) {
      stop("Could not detect base conda environment.")
    }
    if (Sys.info()[["sysname"]] == "Windows") {
      if (grepl("envs", base_python, ignore.case = TRUE)) {
        conda_root <- dirname(dirname(dirname(base_python)))
      } else {
        conda_root <- dirname(base_python)
      }
      conda_bin <- normalizePath(file.path(conda_root, "Scripts", "conda.exe"))
    } else {
      if (grepl("envs", base_python)) {
        conda_root <- dirname(dirname(dirname(base_python)))
      } else {
        conda_root <- dirname(dirname(base_python))
      }
      conda_bin <- normalizePath(file.path(conda_root, "bin", "conda"))
    }
  }

  # Step 3: Decide which Python to use
  base_python <- t$python[t$name %in% c("base", "root")][1]
  base_version <- tryCatch(
    system2(base_python, "--version", stdout = TRUE, stderr = TRUE),
    error = function(e) NA
  )
  use_env <- NULL

  if (!is.na(base_version) && grepl(python_version, base_version)) {
    vmessage("Base conda already provides Python ", python_version)
    py_bin <- base_python
  } else {
    use_env <- paste0("py-", gsub("\\.", "", python_version))
    if (!use_env %in% t$name) {
      vmessage("Creating conda env ", use_env, " with Python ", python_version)
      system2(conda_bin, c("create", "-y", "-n", use_env, paste0("python=", python_version)))
      t <- reticulate::conda_list(conda = conda_bin)
    }
    py_bin <- t$python[t$name == use_env]
  }

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
