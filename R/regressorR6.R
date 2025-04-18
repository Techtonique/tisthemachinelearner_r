#' R6 Class for Scikit-learn Regressors
#'
#' @description
#' An R6 Class that provides an interface to scikit-learn regression models.
#'
#' @export
Regressor <- R6::R6Class(
  "Regressor",
  public = list(
    #' @field model The underlying sklearn model
    model = NULL,
    
    #' @field residuals Model residuals
    residuals = NULL,
    
    #' @field df.residual Degrees of freedom of residuals
    df.residual = NULL,
    
    #' @description
    #' Create a new Regressor object
    #' @param model_name Name of the sklearn model to use
    #' @param ... Additional parameters passed to the sklearn model
    initialize = function(model_name, ...) {
      # Get the model class from sklearn
      model_class <- NULL
      # Check common sklearn modules
      if (is.null(model_class)) model_class <- tisthemachinelearner::sklearn$linear_model$`__dict__`[[model_name]]
      if (is.null(model_class)) model_class <- tisthemachinelearner::sklearn$ensemble$`__dict__`[[model_name]]
      if (is.null(model_class)) model_class <- tisthemachinelearner::sklearn$svm$`__dict__`[[model_name]]
      if (is.null(model_class)) model_class <- tisthemachinelearner::sklearn$tree$`__dict__`[[model_name]]
      if (is.null(model_class)) model_class <- tisthemachinelearner::sklearn$neighbors$`__dict__`[[model_name]]
      
      if (is.null(model_class)) {
        stop(sprintf("Model '%s' not found in common scikit-learn modules. Please check the model name.", model_name))
      }
      
      # Handle empty parameters
      params <- list(...)
      if (length(params) == 0) params <- list()
      
      # Create the model
      private$.model <- do.call(model_class, params)
    },
    
    #' @description
    #' Fit the model to training data
    #' @param x Feature matrix
    #' @param y Target vector
    #' @param calibration Logical flag to indicate if calibration of residuals should be used
    #' @param seed Seed for random number generator
    fit = function(x, y, calibration = FALSE, seed = 42L, ...) {
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
      
      set.seed(seed)
      
      if (!calibration) {
        # Convert inputs to numpy arrays
        #x_np <- reticulate::array_reshape(x, c(nrow(x), ncol(x)))
        #y_np <- reticulate::array_reshape(y, c(length(y), 1))
        
        # Fit the model
        private$.model$fit(x, y, ...)
        
        # Get in-sample predictions and residuals
        y_pred <- private$.model$predict(x)
        self$residuals <- y - y_pred
        self$df.residual <- length(y) - ncol(x) - 1
      } else {
        # Split data into training and calibration sets
        train_idx <- sample(nrow(x), size = floor(0.5 * nrow(x)))
        x_train <- x[train_idx, ]
        x_cal <- x[-train_idx, ]
        y_train <- y[train_idx]
        y_cal <- y[-train_idx]
        
        # Convert training data to numpy arrays
        #x_train_np <- reticulate::array_reshape(x_train, c(nrow(x_train), ncol(x_train)))
        #y_train_np <- reticulate::array_reshape(y_train, c(length(y_train), 1))
        
        # Train model on training set
        private$.model$fit(x_train, y_train)
        
        # Get calibration predictions and residuals
        #x_cal_np <- reticulate::array_reshape(x_cal, c(nrow(x_cal), ncol(x_cal)))
        y_cal_pred <- private$.model$predict(x_cal)
        self$residuals <- y_cal - y_cal_pred
        self$df.residual <- length(y_cal) - ncol(x_cal) - 1
      }
      
      invisible(self)
    },
    
    #' @description
    #' Make predictions on new data
    #' @param newdata New data to predict on
    #' @param method Method for computing prediction intervals
    #' @param nsim Number of simulations for bootstrap/tsbootstrap
    #' @param level Confidence level for prediction intervals
    #' @param seed Random seed
    predict = function(newdata, method = c("none", "splitconformal", "surrogate", 
                                         "bootstrap", "tsbootstrap"),
                      nsim = 250L, level = 95, seed = 123) {
      method <- match.arg(method)
      
      # Convert newdata to numpy array
      #newdata_np <- reticulate::array_reshape(newdata, c(nrow(newdata), ncol(newdata)))
      pred <- as.vector(private$.model$predict(newdata))
      
      if (method == "none") {
        return(pred)
      }
      
      set.seed(seed)
      
      if (method == "bootstrap") {
        errors <- matrix(sample(self$residuals, size = length(pred) * nsim, replace = TRUE), 
                        nrow = length(pred), ncol = nsim)    
      } else if (method == "tsbootstrap") {
        errors <- tseries::tsbootstrap(sample(self$residuals, size = length(pred), replace = TRUE), 
                                     type = "block", nb = nsim)
      } else if (method == "surrogate") {
        errors <- tseries::surrogate(sample(self$residuals, size = length(pred), replace = TRUE), ns = nsim)
      } else { # splitconformal
        multiplier <- quantile(abs(self$residuals), probs = level/100)
        lower <- pred - multiplier
        upper <- pred + multiplier
        return(cbind(fit = drop(pred), lwr = lower, upr = upper))
      }
      
      # Generate simulations
      sims <- errors + drop(pred)
      
      # Compute prediction intervals
      alpha <- (1 - level/100) / 2
      lower <- apply(sims, 1, quantile, probs = alpha)
      upper <- apply(sims, 1, quantile, probs = 1 - alpha)
      
      return(cbind(fit = drop(pred), lwr = lower, upr = upper))
    }
  ),
  
  private = list(
    .model = NULL
  )
)