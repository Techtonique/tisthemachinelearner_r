#'@export
VENV_PATH <- NULL 

#'@export
sklearn <- NULL 
  
#' @import reticulate
.onLoad <- function(libname, pkgname) {
  # Specify the name of the virtual environment
  VENV_PATH <<- "./tisthemachinelearner_env"
  # Create virtual environment if it doesn't exist
  if (!dir.exists(VENV_PATH)) {
    message("Creating Python virtual environment...")
    reticulate::virtualenv_create(VENV_PATH)
    # Install required Python packages
    message("Installing Python packages...")
    reticulate::py_install(c("scikit-learn", "numpy", "pandas"), 
                           envname = VENV_PATH)
  }
  reticulate::use_virtualenv(VENV_PATH)
  sklearn <<- reticulate::import("sklearn")
}