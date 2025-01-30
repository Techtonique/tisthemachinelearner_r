# Initialize the Python tisthemachinelearner module on package load
.onLoad <- function(libname, pkgname) {
  # Check if reticulate is available
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required. Please install it.", call. = FALSE)
  }
  
  # Try to import tisthemachinelearner
  tryCatch({
    tml <- reticulate::import("tisthemachinelearner", delay_load = TRUE)
  }, error = function(e) {
    # If import fails, try to install using pip
    message("Installing Python package 'tisthemachinelearner'...")
    reticulate::py_install("git+https://github.com/Techtonique/tisthemachinelearner.git", pip = TRUE)
  })
  
  # Verify installation was successful
  tryCatch({
    tml <- reticulate::import("tisthemachinelearner")
  }, error = function(e) {
    stop("Failed to install or import 'tisthemachinelearner'. Please install it manually using pip.", call. = FALSE)
  })
}

# Utility function to load Python classes dynamically
load_tisthemachinelearner_class <- function(class_name) {
  if (!exists("tisthemachinelearner", envir = .GlobalEnv)) {
    stop("The tisthemachinelearner module has not been loaded properly.")
  }
  reticulate::py_get_attr(get("tisthemachinelearner", envir = .GlobalEnv), class_name)
}
