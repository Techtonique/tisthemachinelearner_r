#' Get a list of all models in scikit-learn
#'
#' This function retrieves a list of all models available in scikit-learn.
#' It imports the necessary Python modules and retrieves all estimators,
#' filtering them into classifiers and regressors.
#'
#' @return A list with two elements:
#'   - `classifiers`: A character vector of all classifier models
#'   - `regressors`: A character vector of all regressor models
#'
#' @export
#' 
#' @examples
#' 
#' # model_list <- get_model_list()
#' # print(model_list$classifiers)
#' 
get_model_list <- function(venv_path = "venv") {
  
  # Lazy load sklearn only when needed
  sklearn <- get_sklearn(venv_path)
  
  # Import required Python modules through reticulate
  sklearn_utils <- get_sklearn_utils(venv_path)
  sklearn_base <- get_sklearn_base(venv_path)
  
  # Get all estimators from scikit-learn
  estimators <- sklearn_utils$all_estimators()
  
  # Convert Python list of tuples to R lists
  all_models <- reticulate::py_to_r(estimators)
  names <- sapply(all_models, `[[`, 1)
  classes <- sapply(all_models, `[[`, 2)
  
  # Use issubclass to check inheritance
  classifier_idx <- sapply(classes, function(cls) {
    tryCatch({
      reticulate::py_bool(reticulate::py_call(reticulate::import_builtins()$issubclass,
                                             cls,
                                             sklearn_base$ClassifierMixin))
    }, error = function(e) FALSE)
  })
  
  regressor_idx <- sapply(classes, function(cls) {
    tryCatch({
      reticulate::py_bool(reticulate::py_call(reticulate::import_builtins()$issubclass,
                                             cls,
                                             sklearn_base$RegressorMixin))
    }, error = function(e) FALSE)
  })
  
  list(
    classifiers = names[classifier_idx],
    regressors = names[regressor_idx]
  )
}
