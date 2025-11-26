#' Setup Python environment using uv
#' 
#' @param venv_path Path to virtual environment (default: "./venv")
#' @export
#' @examples
#' \dontrun{
#' # After creating venv with: uv venv venv
#' setup_sklearn()
#' }
setup_sklearn <- function(venv_path = "venv") {
  # Check if uv is installed
  if (system2("uv", "--version", stdout = NULL, stderr = NULL) != 0) {
    stop(
      "uv is not installed.\n",
      "Install with: pip install uv\n",
      "Or see: https://github.com/astral-sh/uv"
    )
  }
  
  # Check if venv exists
  if (!dir.exists(venv_path)) {
    stop(
      sprintf("Virtual environment not found at '%s'\n", venv_path),
      "Create it first with:\n",
      sprintf("  uv venv %s\n", venv_path),
      sprintf("  source %s/bin/activate  # Linux/Mac\n", venv_path),
      sprintf("  %s\\Scripts\\activate  # Windows", venv_path)
    )
  }
  
  # Install scikit-learn and dependencies using uv
  message("Installing scikit-learn and dependencies with uv...")
  
  venv_abs <- normalizePath(venv_path, mustWork = TRUE)
  
  result <- system2(
    "uv",
    c("pip", "install", "scikit-learn", "numpy", "scipy"),
    env = paste0("VIRTUAL_ENV=", venv_abs),
    stdout = TRUE,
    stderr = TRUE
  )
  
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    stop("Failed to install Python packages. Check output above.")
  }
  
  message("Setup complete! You can now use regressor() functions.")
  invisible(venv_path)
}