#' R6 Class for Gradient Boosting
#'
#' @description
#' An R6 Class that provides an interface to gradient boosting with neural network feature transformation.
#'
#' @export
Booster <- R6::R6Class(
  "Booster",
  public = list(
    #' @field estimators List of fitted base models
    estimators = NULL,
    
    #' @field learning_rate Learning rate for boosting
    learning_rate = NULL,
    
    #' @field losses Vector of training losses
    losses = NULL,
    
    #' @field n_estimators Number of estimators used
    n_estimators = NULL,
    
    #' @description
    #' Create a new Booster object
    #' @param model_name Name of the base model
    #' @param n_estimators Number of boosting iterations
    #' @param learning_rate Learning rate for boosting
    #' @param tolerance Convergence tolerance
    #' @param calibration Whether to calibrate the model
    #' @param seed Random seed
    #' @param show_progress Whether to show progress bar
    #' @param verbose Whether to print detailed output
    initialize = function(model_name, 
                         n_estimators = 100L,
                         learning_rate = 0.1,
                         tolerance = 1e-4,
                         calibration = FALSE,
                         seed = 123L,
                         show_progress = TRUE,
                         verbose = FALSE) {
      private$.model_name <- model_name
      private$.n_estimators <- n_estimators
      private$.learning_rate <- learning_rate
      private$.tolerance <- tolerance
      private$.calibration <- calibration
      private$.seed <- seed
      private$.show_progress <- show_progress
      private$.verbose <- verbose
    },
    
    #' @description
    #' Fit the boosting model to training data
    #' @param x Feature matrix
    #' @param y Target vector
    fit = function(x, y) {
      # Input validation
      if (!is.matrix(x) && !is.data.frame(x)) {
        stop("'x' must be a matrix or data frame")
      }
      if (!is.vector(y)) {
        stop("'y' must be a vector")
      }
      if (length(y) != nrow(x)) {
        stop("Length of 'y' must match number of rows in 'x'")
      }
      
      # Fit the model using boosterCpp
      result <- boosterCpp(
        as.matrix(x), y, 
        private$.model_name,
        private$.n_estimators,
        private$.learning_rate,
        private$.tolerance,
        private$.calibration,
        private$.seed,
        private$.show_progress,
        private$.verbose
      )
      
      # Store results in public fields
      self$estimators <- result$estimators
      self$learning_rate <- result$learning_rate
      self$losses <- result$losses
      self$n_estimators <- result$n_estimators
      
      invisible(self)
    },
    
    #' @description
    #' Make predictions on new data
    #' @param newdata New data to predict on
    predict = function(newdata) {
      if (!is.matrix(newdata) && !is.data.frame(newdata)) {
        stop("'newdata' must be a matrix or data frame")
      }
      
      # Create a list with the necessary components for predictBoosterCpp
      booster_list <- list(
        estimators = self$estimators,
        learning_rate = self$learning_rate,
        n_estimators = self$n_estimators,
        show_progress = private$.show_progress
      )
      class(booster_list) <- "booster"
      
      # Make predictions using predictBoosterCpp
      predictBoosterCpp(booster_list, as.matrix(newdata))
    }
  ),
  
  private = list(
    .model_name = NULL,
    .n_estimators = NULL,
    .learning_rate = NULL,
    .tolerance = NULL,
    .calibration = NULL,
    .seed = NULL,
    .show_progress = NULL,
    .verbose = NULL
  )
) 