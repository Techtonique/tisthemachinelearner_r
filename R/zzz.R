.onLoad <- function(libname, pkgname) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required. Please install it.", call. = FALSE)
  }

  # Set the Conda environment
  reticulate::use_condaenv("r-reticulate", required = TRUE)

  # Try importing the package
  tml <- tryCatch({
    reticulate::import("tisthemachinelearner", delay_load = TRUE)
  }, error = function(e) {
    message("Python package 'tisthemachinelearner' not found in 'r-reticulate'. Installing via pip...")

    # Install in the r-reticulate Conda environment
    system("conda run -n r-reticulate python -m pip install --no-cache-dir --upgrade git+https://github.com/Techtonique/tisthemachinelearner.git", intern = TRUE)

    # Try importing again after installation
    tryCatch({
      tml <- reticulate::import("tisthemachinelearner")
    }, error = function(e2) {
      stop("Failed to install or import 'tisthemachinelearner' in 'r-reticulate'. Please install it manually.", call. = FALSE)
    })
  })

  assign("tisthemachinelearner", tml, envir = .GlobalEnv)
}
