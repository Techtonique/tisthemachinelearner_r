#' @export
VENV_PATH <- NULL 

#' @export
sklearn <- NULL  
#' @export 
numpy <- NULL
#' @export
pandas <- NULL

#' @import reticulate
.onLoad <- function(libname, pkgname) {

  tryCatch({
  # Specify the name of the virtual environment
  env_name <- "tisthemachinelearner_env"
  
  # Path to the virtual environment (persistent location)
  VENV_PATH <<- file.path(Sys.getenv("HOME"), ".tisthemachinelearner", env_name)
  
  # Create the parent directory if it doesn't exist
  if (!dir.exists(dirname(VENV_PATH))) {
    dir.create(dirname(VENV_PATH), recursive = TRUE, showWarnings = FALSE)
  }
  
  # Check if the virtual environment exists
  if (!dir.exists(VENV_PATH)) {
    message("Creating Python virtual environment...")
    
    # Create the virtual environment
    tryCatch({
      reticulate::virtualenv_create(VENV_PATH, python = "python3")
    }, error = function(e) {
      stop("Failed to create virtual environment: ", e$message)
    })
    
    # Install required Python packages
    message("Installing Python packages...")
    tryCatch({
      reticulate::virtualenv_install(
        VENV_PATH,
        packages = c("scikit-learn", "numpy", "pandas"),
        ignore_installed = TRUE
      )
    }, error = function(e) {
      stop("Failed to install Python packages: ", e$message)
    })
  }  
  # Use the virtual environment
  tryCatch({
    reticulate::use_virtualenv(VENV_PATH, required = TRUE)
  }, error = function(e) {
    stop("Failed to use virtual environment: ", e$message)
  })  
  # Verify the installed packages
  py_config <- reticulate::py_config()
  message("Using Python environment: ", py_config$python)
 }, error = function(e) {
    message("Using system Python environment: ") # e.g on Colab
  })  
  # Import sklearn lazily
  tryCatch({
    message("Importing sklearn from Global Env...")
    sklearn <<- reticulate::import("sklearn", delay_load = TRUE)
  }, error = function(e) {
    message("Failed to import sklearn from Global Env: ", e$message)
    message("Installing sklearn...")
    reticulate::py_install("scikit-learn")
    reticulate::py_install("numpy")
    reticulate::py_install("pandas")
    reticulate::use_virtualenv("r-reticulate", required = TRUE)
    message("Re-importing sklearn...")
    sklearn <<- reticulate::import("sklearn", delay_load = TRUE)
    numpy <<- reticulate::import("numpy", delay_load = TRUE)
    pandas <<- reticulate::import("pandas", delay_load = TRUE)
  })
}