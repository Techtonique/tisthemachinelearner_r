#' @import R6
#' @export 
Regressor <- R6::R6Class(
  classname = "Regressor",
  public = list(
    #' @field model The fitted model object
    model = NULL,
    #' @field model_name Name of the model to use
    model_name = NULL,

    #' @description
    #' Create a new Regressor instance
    #' @param model_name Name of the model to use
    #' @param ... Additional parameters for the model
    #' @return A new `Regressor` object
    initialize = function(model_name = "LinearRegression", ...) {
      tml <- reticulate::import("tisthemachinelearner")
      self$model_name <- model_name
      self$model <- tml$Regressor(model_name, ...)
    },

    #' @description
    #' Fit the model to training data
    #' @param x Feature matrix
    #' @param y Target variable
    #' @return self (for method chaining)
    fit = function(x, y) {
      self$model$fit(x, y)
      invisible(self)
    },

    #' @description
    #' Make predictions on new data
    #' @param x Feature matrix for predictions
    #' @return Vector of predictions
    predict = function(x) {
      if (is.null(self$model)) stop("Model must be fitted before predicting")
      self$model$predict(x)
    }
  )
)
