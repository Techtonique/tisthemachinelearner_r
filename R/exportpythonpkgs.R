
#' @export  
get_sklearn <- function(venv_path = "./venv") {
  # Use the specified virtual environment
  reticulate::use_virtualenv(venv_path, 
                             required = TRUE)
  
  # Lazy load sklearn only when needed
  reticulate::import("sklearn", 
                      delay_load = TRUE)
}

#' @export  
get_sklearn_utils <- function(venv_path = "./venv") {
  # Use the specified virtual environment
  reticulate::use_virtualenv(venv_path, 
                             required = TRUE)
  
  # Lazy load sklearn only when needed
  reticulate::import("sklearn.utils", 
                     delay_load = TRUE)
}

#' @export  
get_sklearn_base <- function(venv_path = "./venv") {
  # Use the specified virtual environment
  reticulate::use_virtualenv(venv_path, 
                             required = TRUE)
  
  # Lazy load sklearn only when needed
  reticulate::import("sklearn.base", 
                     delay_load = TRUE)
}

