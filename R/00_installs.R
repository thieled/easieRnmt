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





#' @title Install PyTorch in a Conda Virtual Environment
#'
#' @description Installs PyTorch and related libraries (`torch`, `torchvision`, `torchaudio`)
#' in a Conda environment created with [install_conda_venv()]. The function detects CUDA/GPU
#' availability and installs the appropriate build. If the GPU build URL is invalid, it falls back to CPU.
#'
#' @param venv_name Character. Name of the virtual environment. Default "r-easynmt".
#' @param conda_path Character. Path to Conda installation. If NULL, autodetect.
#' @param python_version Character. Python version used in the environment. Default "3.11".
#' @param verbose Logical. If TRUE, prints progress messages. Default TRUE.
#'
#' @return Invisibly returns `NULL`.
#' @export
install_torch <- function(venv_name = "r-easynmt",
                          conda_path = NULL,
                          python_version = "3.11",
                          verbose = TRUE) {

  vmessage <- function(...) if (verbose) message(...)

  # Make sure the environment exists
  install_conda_venv(
    python_version = python_version,
    venv_name = venv_name,
    ask = TRUE,
    force = FALSE,
    verbose = verbose,
    conda_path = conda_path
  )

  # Detect GPU
  gpu_available <- FALSE
  if (.Platform$OS.type == "windows") {
    gpu_info <- try(system("wmic path win32_VideoController get name", intern = TRUE), silent = TRUE)
    gpu_available <- any(grepl("NVIDIA", gpu_info, ignore.case = TRUE))
  } else {
    gpu_info <- try(system("nvidia-smi", intern = TRUE), silent = TRUE)
    gpu_available <- !inherits(gpu_info, "try-error") && length(gpu_info) > 0
  }

  # Detect CUDA version if GPU present
  cuda_version <- NULL
  if (gpu_available) {
    nvcc_output <- tryCatch(system("nvcc --version", intern = TRUE), error = function(e) NULL)
    if (!is.null(nvcc_output)) {
      version_line <- nvcc_output[grepl("release", nvcc_output)]
      cuda_version <- sub(".*release ([0-9]+\\.[0-9]+).*", "\\1", version_line)
    }
  }

  # Construct install URL
  base_url <- "https://download.pytorch.org/whl/"
  index_url <- if (is.null(cuda_version)) {
    paste0(base_url, "cpu")
  } else {
    paste0(base_url, "cu", gsub("\\.", "", cuda_version))
  }

  # Validate URL
  validate_url <- function(url) {
    if (!requireNamespace("httr", quietly = TRUE)) stop("Package 'httr' is required.")
    response <- tryCatch(httr::HEAD(url), error = function(e) NULL)
    !is.null(response) && httr::status_code(response) == 200
  }

  if (!validate_url(index_url)) {
    cli::cli_warn("Invalid or unavailable PyTorch URL: {index_url}. Falling back to CPU-only build.")
    index_url <- paste0(base_url, "cpu")
    cuda_version <- NULL
  }

  vmessage("Installing PyTorch into environment '", venv_name, "' ...")
  if (is.null(cuda_version)) {
    vmessage("No CUDA detected. Installing CPU-only build.")
  } else {
    vmessage("CUDA ", cuda_version, " detected. Installing GPU build.")
  }

  # Install via reticulate
  tryCatch({
    reticulate::py_install(
      packages = c("torch", "torchvision", "torchaudio"),
      pip = TRUE,
      envname = venv_name,
      pip_options = paste("--index-url", index_url)
    )
    vmessage("Torch installation completed successfully.")
  }, error = function(e) {
    stop("Torch installation failed: ", e$message)
  })

  # Verify installation
  tryCatch({
    torch <- reticulate::import("torch", delay_load = TRUE)
    vmessage("Torch successfully installed and imported.")
    vmessage("Torch version: ", torch$`__version__`)
    vmessage("CUDA available: ", torch$cuda$is_available())
    if (!is.null(cuda_version)) {
      vmessage("CUDA version (from torch): ", torch$version$cuda)
    }
  }, error = function(e) {
    stop("Verification failed: Torch is not properly installed.")
  })

  invisible(NULL)
}




#' Install FastText in a Python virtual environment
#'
#' This function installs the FastText library inside a conda virtual
#' environment managed through \code{reticulate}. On Windows, a precompiled
#' wheel is downloaded, unpacked, and installed. On Linux and macOS, the
#' library is installed directly from PyPI. If installation fails (for example
#' due to unsupported Python versions), the function does not stop but issues
#' a warning instead.
#'
#' @param wheel_url Character string. URL to the zipped FastText wheel for
#'   Windows. Defaults to the official release of version 0.9.2 for Python 3.11
#'   and 64-bit Windows.
#' @param python_version Character string. Python version to use when creating
#'   the conda environment via \code{easieRnmt::install_conda_venv}. Default is
#'   \code{"3.11"}.
#' @param venv_name Character string. Name of the conda virtual environment.
#'   Default is \code{"r-easynmt"}.
#' @param verbose Logical. If \code{TRUE}, print progress messages during
#'   installation. Default is \code{TRUE}.
#' @param conda_path Optional character string. Path to a conda installation.
#'   If \code{NULL}, the default conda installation is used.
#' @param force Logical. If \code{TRUE}, uninstall any existing installations of
#'   \code{fasttext} or \code{fasttext-wheel} before reinstalling. Default is
#'   \code{FALSE}.
#'
#' @return Invisibly returns \code{TRUE} if the function completes. If
#'   installation fails, warnings are raised but the process continues.
#'
#' @details
#' On Windows, FastText is not officially supported and compilation from
#' source often fails. This function therefore installs a precompiled wheel
#' distributed as a zipped file. On Linux and macOS, the package is installed
#' directly from PyPI with \code{pip install fasttext}.
#'
#' @export
install_fasttext <- function(
    wheel_url = "https://github.com/facebookresearch/fastText/files/14355061/fasttext-0.9.2-cp311-cp311-win_amd64.whl.zip",
    python_version = "3.11",
    venv_name = "r-easynmt",
    verbose = TRUE,
    conda_path = NULL,
    force = FALSE
) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Please install reticulate first.")
  }

  # ensure venv exists / is activated
  easieRnmt::install_conda_venv(
    force = FALSE,
    python_version = python_version,
    venv_name = venv_name,
    verbose = verbose,
    conda_path = conda_path
  )
  py_bin <- reticulate::py_exe()

  # cleanup if requested
  if (force) {
    tryCatch({
      system(sprintf('"%s" -m pip uninstall -y fasttext fasttext-wheel', py_bin))
    }, error = function(e) {
      warning("Uninstall step failed: ", conditionMessage(e))
    })
  }

  os <- tolower(Sys.info()[["sysname"]])

  if (os == "windows") {
    # --- Windows: use precompiled wheel ---
    tmp_zip <- tempfile(fileext = ".zip")
    tmp_dir <- tempfile()
    dir.create(tmp_dir, recursive = TRUE)

    tryCatch({
      utils::download.file(wheel_url, tmp_zip, mode = "wb", quiet = !verbose)
      utils::unzip(tmp_zip, exdir = tmp_dir)
      wheel_file <- list.files(tmp_dir, pattern = "\\.whl$", full.names = TRUE)
      if (length(wheel_file) == 0) stop("No .whl file found in zip")
      cmd <- sprintf('"%s" -m pip install "%s"', py_bin, wheel_file)
      if (verbose) message("Running: ", cmd)
      system(cmd)
    }, error = function(e) {
      warning("FastText installation from wheel failed: ", conditionMessage(e))
    })

  } else {
    # --- Linux / macOS: install from PyPI ---
    tryCatch({
      cmd <- sprintf('"%s" -m pip install fasttext', py_bin)
      if (verbose) message("Running: ", cmd)
      system(cmd)
    }, error = function(e) {
      warning("FastText installation via pip failed: ", conditionMessage(e))
    })
  }

  invisible(TRUE)
}


