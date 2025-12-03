#' Fit a boosting model with neural network feature transformation
#' 
#' @param x Input matrix
#' @param y Target vector
#' @param model_name Name of the base model
#' @param n_estimators Number of boosting iterations
#' @param learning_rate Learning rate for boosting
#' @param tolerance Convergence tolerance
#' @param calibration Whether to calibrate the model
#' @param seed Random seed
#' @param show_progress Whether to show progress bar
#' @param verbose Whether to print detailed output
#' @param ... Additional arguments passed to the regressor
#' 
#' @export
booster <- function(x, y, model_name="ExtraTreeRegressor", 
                    n_estimators = 100L, 
                    learning_rate = 0.1,
                    tolerance = 1e-4,
                    calibration = FALSE, 
                    seed = 123L, 
                    show_progress = TRUE,
                    verbose = FALSE,
                    venv_path = "./venv",
                    ...) {  
  
    # Use the specified virtual environment
    reticulate::use_virtualenv(venv_path, 
                               required = TRUE)

    # Lazy load sklearn only when needed
    sklearn <- reticulate::import("sklearn", 
                                  delay_load = TRUE)
    
    boosterCpp(as.matrix(x), y, model_name, 
               n_estimators, learning_rate, tolerance,
               calibration, seed, show_progress, verbose, 
               venv_path)
}

#' Predict using a boosted model
#' 
#' @param object A boosted model object
#' @param newdata New data to predict on
#' @param ... Additional arguments
#' @export
predict.booster <- function(object, newdata, ...) {
    predictBoosterCpp(object, as.matrix(newdata))
}   