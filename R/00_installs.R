
#' @title Install and Set Up a Conda Environment
#'
#' @description
#' Ensures Miniconda is installed, creates (or recreates) a Conda environment
#' with the specified name and Python version, and activates it.
#' This function does not handle fastText installation, as the R package
#' \pkg{fastText} should be used instead.
#'
#' @param conda_env_name Character. The name of the Conda environment to create or use.
#'   Defaults to `"r-easynmt"`.
#' @param python_version Character. Python version to use when creating the Conda environment.
#'   Defaults to `"3.11"`. If the environment already exists, the version is not changed
#'   unless \code{force = TRUE}.
#' @param conda_path Character. Optional path to a Miniconda installation. If `NULL`,
#'   autodetects or installs Miniconda to the default location.
#' @param ask Logical. If `TRUE`, prompts the user before installing Miniconda.
#'   Default is `TRUE`.
#' @param force Logical. If `TRUE`, removes an existing Conda environment with the same
#'   name before recreating it with the requested Python version. Default is `FALSE`.
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `TRUE`.
#'
#' @return Invisibly returns `NULL`. Called for its side effects of installing and
#' configuring the Conda environment.
#'
#' @export
install_conda_env <- function(conda_env_name = "r-easynmt",
                              python_version = "3.11",
                              conda_path = NULL,
                              ask = TRUE,
                              force = FALSE,
                              verbose = TRUE) {
  vmessage <- function(...) if (verbose) message(...)

  # Step 1: Ensure Miniconda is installed
  t <- tryCatch({
    if (!is.null(conda_path)) {
      reticulate::conda_list(conda = file.path(conda_path, "bin", "conda"))
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
  }

  # Step 2: Check if env exists
  env_exists <- if (!is.null(conda_path)) {
    reticulate::condaenv_exists(envname = conda_env_name,
                                conda = file.path(conda_path, "bin", "conda"))
  } else {
    reticulate::condaenv_exists(envname = conda_env_name)
  }

  # Step 3: Remove env if force = TRUE and it exists
  if (env_exists && force) {
    vmessage("Removing existing conda environment: ", conda_env_name)
    if (!is.null(conda_path)) {
      reticulate::conda_remove(envname = conda_env_name,
                               conda = file.path(conda_path, "bin", "conda"))
    } else {
      reticulate::conda_remove(envname = conda_env_name)
    }
    env_exists <- FALSE
  }

  # Step 4: Create env if missing
  if (!env_exists) {
    vmessage("Creating conda environment: ", conda_env_name)
    if (!is.null(conda_path)) {
      reticulate::conda_create(envname = conda_env_name,
                               conda = file.path(conda_path, "bin", "conda"),
                               python_version = python_version)
    } else {
      reticulate::conda_create(envname = conda_env_name,
                               python_version = python_version)
    }
  } else {
    vmessage("Conda environment already exists: ", conda_env_name)
  }


  # Step 4: Activate env
  vmessage("Activating conda environment: ", conda_env_name)
  if (!is.null(conda_path)) {
    reticulate::use_condaenv(conda_env_name, required = TRUE,
                             conda = file.path(conda_path, "bin", "conda"))
  } else {
    reticulate::use_condaenv(conda_env_name, required = TRUE)
  }

  invisible(NULL)
}


#' @title Install PyTorch in a Conda Environment
#'
#' @description Installs PyTorch and related libraries (`torch`, `torchvision`, `torchaudio`)
#' in a Conda environment created with [install_conda_env()]. The function detects CUDA/GPU
#' availability and installs the appropriate build. If the GPU build URL is invalid, it falls back to CPU.
#'
#' @param conda_env_name Character. Name of the conda environment. Default "r-easynmt".
#' @param python_version Character. Python version to use when creating the Conda environment.
#'   Defaults to `"3.11"`. If the environment already exists, the version is not changed
#'   unless \code{force = TRUE}.
#' @param conda_path Character. Path to Conda installation. If NULL, autodetect.
#' @param verbose Logical. If TRUE, prints progress messages. Default TRUE.
#'
#' @return Invisibly returns `NULL`.
#' @export
install_torch <- function(conda_env_name = "r-easynmt",
                          python_version = "3.11",
                          conda_path = NULL,
                          verbose = TRUE) {

  vmessage <- function(...) if (verbose) message(...)

  # Make sure the environment exists
  install_conda_env(
    conda_env_name = conda_env_name,
    python_version = python_version,
    conda_path = conda_path,
    ask = TRUE,
    force = FALSE,
    verbose = verbose
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

  vmessage("Installing PyTorch into conda environment '", conda_env_name, "' ...")
  if (is.null(cuda_version)) {
    vmessage("No CUDA detected. Installing CPU-only build.")
  } else {
    vmessage("CUDA ", cuda_version, " detected. Installing GPU build.")
  }

  # Install via reticulate into conda env
  tryCatch({
    reticulate::py_install(
      packages = c("torch", "torchvision", "torchaudio"),
      envname = conda_env_name,
      method = "conda",
      pip = TRUE,
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




#' @title Install FastText in a Conda Environment
#'
#' @description
#' Installs the FastText library inside a conda environment managed through
#' \code{reticulate}. On Windows, a precompiled wheel is downloaded, unpacked,
#' and installed. On Linux and macOS, the library is installed directly from PyPI.
#' If installation fails, the function does not stop but issues a warning instead.
#'
#' @param wheel_url Character string. URL to the zipped FastText wheel for Windows.
#'   Defaults to the official release of version 0.9.2 for Python 3.11 and 64-bit Windows.
#' @param python_version Character string. Python version to use when creating
#'   the conda environment via \code{install_conda_env}. Default is \code{"3.11"}.
#' @param conda_env_name Character string. Name of the conda environment.
#'   Default is \code{"r-easynmt"}.
#' @param verbose Logical. If \code{TRUE}, print progress messages during installation.
#'   Default is \code{TRUE}.
#' @param conda_path Optional character string. Path to a conda installation.
#'   If \code{NULL}, the default conda installation is used.
#' @param force Logical. If \code{TRUE}, uninstall any existing installations of
#'   \code{fasttext} before reinstalling. Default is \code{FALSE}.
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
    wheel_url = paste0(
      "https://github.com/facebookresearch/fastText/files/",
      "14355061/fasttext-0.9.2-cp311-cp311-win_amd64.whl.zip"
    ),
    python_version = "3.11",
    conda_env_name = "r-easynmt",
    verbose = TRUE,
    conda_path = NULL,
    force = FALSE
) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Please install reticulate first.")
  }

  # ensure conda env exists / is activated
  install_conda_env(
    conda_env_name = conda_env_name,
    python_version = python_version,
    conda_path = conda_path,
    ask = TRUE,
    force = FALSE,
    verbose = verbose
  )
  py_bin <- reticulate::py_exe()

  # cleanup if requested
  if (force) {
    tryCatch({
      system(sprintf('"%s" -m pip uninstall -y fasttext', py_bin))
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



#' @title Install Python Dependencies for easieRnmt
#' @description Installs required Python dependencies into the active environment.
#' This function can be extended with additional packages as needed.
#' @param packages Character vector of Python packages to install.
#'   Default includes "pytz" and "python-dateutil".
#' @param envname Name of the conda or virtual environment where packages
#'   should be installed. If NULL, installs into the active environment.
#' @param method Installation method passed to reticulate::py_install
#'   ("auto", "virtualenv", or "conda").
#' @param pip Logical; if TRUE, force installation via pip.
#' @param ... Further arguments passed to reticulate::py_install().
#' @export
install_deps <- function(
    packages = c("pytz", "python-dateutil"),
    envname = NULL,
    method = "auto",
    pip = TRUE,
    ...
) {
  tryCatch({
    reticulate::py_install(
      packages = packages,
      envname = envname,
      method = method,
      pip = pip,
      ...
    )
    message("Successfully installed Python dependencies: ", paste(packages, collapse = ", "))
  }, error = function(e) {
    warning("Failed to install dependencies: ", conditionMessage(e))
  })
}




#' @title Install EasyNMT in a Conda Environment
#'
#' @description
#' Installs the EasyNMT library and its dependencies in a conda environment
#' managed through \code{reticulate}. It creates or activates the specified
#' environment, installs PyTorch (with CUDA if available, otherwise CPU),
#' installs FastText (using a precompiled wheel on Windows), and finally
#' installs EasyNMT with all required dependencies.
#'
#' @param python_version Character string. Python version to use when creating
#'   the conda environment. Default is \code{"3.11"}.
#' @param conda_env_name Character string. Name of the conda environment to
#'   create or activate. Default is \code{"r-easynmt"}.
#' @param ask Logical. If \code{TRUE}, ask before installing Miniconda. Default
#'   is \code{TRUE}.
#' @param force Logical. If \code{TRUE}, force reinstallation of the conda
#'   environment and Python packages. Default is \code{TRUE}.
#' @param verbose Logical. If \code{TRUE}, print progress messages during
#'   installation. Default is \code{TRUE}.
#' @param conda_path Optional character string. Path to a conda installation. If
#'   \code{NULL}, the default conda installation is used.
#'
#' @return Invisibly returns \code{TRUE} if the installation process completes.
#'   If FastText cannot be installed or imported, the function will still
#'   attempt to install EasyNMT without FastText and issue a warning.
#'
#' @details
#' On Windows, FastText is not officially supported and compilation from source
#' often fails. The helper function \code{install_fasttext} installs a
#' precompiled wheel, which only works for Python versions up to 3.11. On Linux
#' and macOS, FastText can be installed directly from PyPI.
#'
#' The function installs the following dependencies if FastText is not
#' available: \code{nltk}, \code{numpy}, \code{protobuf}, \code{sentencepiece},
#' \code{torch}, \code{tqdm}, \code{transformers}, \code{langdetect},
#' \code{sacremoses}.
#'
#' @export
install_easynmt <- function(
    python_version = "3.11",
    conda_env_name = "r-easynmt",
    ask = TRUE,
    force = TRUE,
    verbose = TRUE,
    conda_path = NULL
) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Please install reticulate first.")
  }

  # 1. Install or activate conda env (installs miniconda if not present)
  install_conda_env(
    python_version = python_version,
    conda_env_name = conda_env_name,
    ask = ask,
    force = force,
    verbose = verbose,
    conda_path = conda_path
  )

  # 2. Install PyTorch, with automatic CUDA or CPU support
  install_torch(conda_env_name = conda_env_name,
                conda_path = conda_path,
                verbose = verbose)

  # 3. Install FastText (prebuilt wheel for Windows)
  install_fasttext(python_version = python_version,
                   conda_env_name = conda_env_name,
                   verbose = verbose,
                   conda_path = conda_path,
                   force = TRUE)

  # 4. Check if FastText works
  py_bin <- reticulate::py_exe()
  check_code <- "
try:
    import fasttext
    ok = hasattr(fasttext, 'train_unsupervised')
    print('OK' if ok else 'FAIL')
except Exception:
    print('FAIL')
"
  tmpfile <- tempfile(fileext = ".py")
  writeLines(check_code, tmpfile)
  result <- system(sprintf('"%s" "%s"', py_bin, tmpfile), intern = TRUE)
  unlink(tmpfile)

  fasttext_ok <- any(grepl("^OK$", result))

  # 5. Install EasyNMT (+ deps) depending on FastText availability
  if (fasttext_ok) {
    message("FastText detected. Installing EasyNMT with dependencies...")
    cmd <- sprintf(
      '"%s" -m pip install easynmt nltk numpy protobuf sentencepiece torch tqdm transformers langdetect sacremoses',
      py_bin
    )
    system(cmd)
  } else {
    warning("FastText not detected. Installing EasyNMT without FastText and adding other dependencies...")
    cmd1 <- sprintf('"%s" -m pip install easynmt --no-deps', py_bin)
    cmd2 <- sprintf(
      '"%s" -m pip install nltk numpy protobuf sentencepiece torch tqdm transformers langdetect sacremoses',
      py_bin
    )
    system(cmd1)
    system(cmd2)
  }

  # 6. Install additional dependencies via install_deps()
  install_deps(
    packages = c("pytz", "python-dateutil"),  # extend this vector later
    envname  = conda_env_name,
    method   = "conda",
    pip      = TRUE
  )

  # 7. Mark EasyNMT as initialized
  options("easynmt_initialized" = TRUE)

  invisible(TRUE)
}



#' @title Initialize the r-easynmt Conda Environment
#'
#' @description
#' Initializes and verifies the Python environment used by \pkg{easieRnmt}.
#' It builds on \code{\link{install_easynmt}} by activating the specified
#' conda environment, checking the Python configuration, and verifying that
#' required Python packages are correctly installed.
#'
#' Specifically, the function:
#' \itemize{
#'   \item Ensures that the Python environment created by
#'   \code{install_easynmt()} is active.
#'   \item Prints the active environment, Python path, and Python version.
#'   \item Verifies that PyTorch is installed and reports its version as
#'   well as whether CUDA is available.
#'   \item Checks whether \code{fasttext} and \code{EasyNMT} are installed
#'   and reports their versions.
#'   \item Sets a global option (\code{easynmt_initialized = TRUE}) so that
#'   subsequent calls can skip redundant initialization.
#' }
#'
#' @param python_version Character scalar. Python version to use for the
#' environment. Default is \code{"3.11"}.
#' @param conda_env_name Character scalar. Name of the conda environment to
#' create or activate. Default is \code{"r-easynmt"}.
#' @param ask Logical. If \code{TRUE}, prompt the user before creating or
#' overwriting an environment. Default is \code{TRUE}.
#' @param force Logical. If \code{TRUE}, force reinstallation of the
#' environment even if it already exists. Default is \code{FALSE}.
#' @param verbose Logical. If \code{TRUE}, print informative messages about
#' the environment configuration and installed packages. Default is
#' \code{TRUE}.
#' @param conda_path Optional character scalar. Path to the conda
#' installation to use. If \code{NULL}, the default conda installation is
#' used.
#'
#' @return This function is called for its side effects. It prints messages
#' about the environment and package versions if \code{verbose = TRUE}, and
#' sets the global option \code{easynmt_initialized = TRUE}.
#'
#' @export
initialize_easynmt <- function(python_version = "3.11",
                               conda_env_name = "r-easynmt",
                               ask = TRUE,
                               force = FALSE,
                               verbose = TRUE,
                               conda_path = NULL) {

  vmessage <- function(...) if (verbose) message(...)

  if (!is.null(options("easynmt_initialized")$easynmt_initialized)) {
    vmessage("r-easynmt environment is already initialized.")
  } else {

    install_conda_env(python_version = python_version,
                      conda_env_name = conda_env_name,
                      ask = ask,
                      force = force,
                      verbose = verbose,
                      conda_path = conda_path)

    # Check if torch is available
    if (!reticulate::py_module_available("torch")) {
      install_torch(conda_env_name = conda_env_name,
                    conda_path = conda_path,
                    verbose = verbose)
    }

    # Check if easynmt is available
    if (!reticulate::py_module_available("easynmt")) {
      install_easynmt(python_version = python_version,
                      conda_env_name = conda_env_name,
                      ask = ask,
                      force = force,
                      verbose = verbose,
                      conda_path = conda_path)
    }
  }

  # Verify Python installation
  cfg <- tryCatch(reticulate::py_config(), error = function(e) NULL)
  python <- if (!is.null(cfg)) cfg$python else "not initialized"
  libpython <- if (!is.null(cfg)) cfg$libpython else "not initialized"
  version <- if (is.null(cfg) || all(is.na(cfg$version))) "unknown" else
    if (is.character(cfg$version)) cfg$version else paste(cfg$version, collapse = ".")

  vmessage("Python: ", python)
  vmessage("Libpython: ", libpython)
  vmessage("Python Version: ", version)

  # Verify PyTorch installation
  tryCatch({
    torch <- reticulate::import("torch")
    vmessage("Torch version: ", torch$`__version__`)
    vmessage("CUDA available: ", torch$cuda$is_available())
  }, error = function(e) {
    stop("Verification failed: Torch is not properly installed.")
  })

  # Helper to get Python package version via importlib.metadata
  get_py_version <- function(pkg) {
    tryCatch(
      {
        v <- reticulate::py_run_string(
          sprintf("import importlib.metadata as im; v = im.version('%s')", pkg)
        )
        reticulate::py_to_r(v$v)
      },
      error = function(e) NA_character_
    )
  }

  easynmt_ver  <- get_py_version("easynmt")
  fasttext_ver <- get_py_version("fasttext")

  if (!is.na(fasttext_ver)) {
    vmessage("fasttext version: ", fasttext_ver)
  } else {
    vmessage("fasttext not found in active Python environment.")
  }

  if (!is.na(easynmt_ver)) {
    vmessage("EasyNMT version: ", easynmt_ver)
  } else {
    vmessage("EasyNMT not found in active Python environment.")
  }

  # Source the Python translation script so easynmt_translate is available
  pyfile <- system.file("python", "easynmt_translate.py", package = "easieRnmt", mustWork = TRUE)
  reticulate::source_python(pyfile)

  if (!"easynmt_translate" %in% reticulate::py_list_attributes(reticulate::py)) {
    stop("'easynmt_translate' function could not be sourced from Python.")
  }

  options("easynmt_initialized" = TRUE)
}

