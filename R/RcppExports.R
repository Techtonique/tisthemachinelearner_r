# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

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
#'
#' @export
boosterCpp <- function(x, y, model_name, n_estimators = 100L, learning_rate = 0.1, tolerance = 1e-4, calibration = FALSE, seed = 123L, show_progress = TRUE, verbose = FALSE) {
    .Call(`_tisthemachinelearner_boosterCpp`, x, y, model_name, n_estimators, learning_rate, tolerance, calibration, seed, show_progress, verbose)
}

#' Predict using a boosted model
#'
#' @param booster A boosted model object
#' @param x New data to predict on
#'
#' @export
predictBoosterCpp <- function(booster, x) {
    .Call(`_tisthemachinelearner_predictBoosterCpp`, booster, x)
}

