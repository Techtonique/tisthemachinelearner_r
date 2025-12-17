#' Create a regression model using scikit-learn
#' 
#' @param x Feature matrix
#' @param y Target vector
#' @param model_name Name of the sklearn model to use
#' @param calibration Logical flag to indicate if calibration of residuals should be used
#' @param seed Seed for the random number generator
#' @param venv_path Path to Python virtual environment
#' @param ... Additional parameters passed to the sklearn model
#' @return A regressor object
#' @export
#' @examples
#' 
#' \dontrun{
#' # Split features and target
#' X <- as.matrix(mtcars[, -1])  # all columns except mpg
#' y <- mtcars[, 1]              # mpg column
#' 
#' # Create train/test split
#' set.seed(42)
#' train_idx <- sample(nrow(mtcars), size = floor(0.8 * nrow(mtcars)))
#' X_train <- X[train_idx, ]
#' X_test <- X[-train_idx, ]
#' y_train <- y[train_idx]
#' y_test <- y[-train_idx]
#' 
#' # Fit linear regression model
#' reg_linear <- regressor(X_train, y_train, "LinearRegression")
#' 
#' # Make predictions
#' predictions <- predict(reg_linear, X_test)
#' 
#' # Calculate RMSE
#' (rmse <- sqrt(mean((predictions - y_test)^2)))
#' }
#' 
regressor <- function(x, y, model_name, 
                      calibration = FALSE, 
                      seed = 42L, 
                      venv_path = "./venv",
                      ...) {
  
  # Lazy load sklearn only when needed
  sklearn <- get_sklearn(venv_path)
  
  # Input validation
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("'x' must be a matrix or data frame")
  }

  # Handle empty parameters
  params <- list(...)
  if (length(params) == 0) params <- list()
  
  # Convert inputs to numpy arrays
  #x_np <- reticulate::array_reshape(x, c(nrow(x), ncol(x)))
  #y_np <- reticulate::array_reshape(y, c(length(y), 1))
  
  # Get the model class from sklearn
  model_class <- NULL
  # Check common sklearn modules
  if (is.null(model_class)) model_class <- sklearn$cross_decomposition$`__dict__`[[model_name]]
  if (is.null(model_class)) model_class <- sklearn$isotonic$`__dict__`[[model_name]]
  if (is.null(model_class)) model_class <- sklearn$kernel_ridge$`__dict__`[[model_name]]
  if (is.null(model_class)) model_class <- sklearn$linear_model$`__dict__`[[model_name]]
  if (is.null(model_class)) model_class <- sklearn$ensemble$`__dict__`[[model_name]]
  if (is.null(model_class)) model_class <- sklearn$gaussian_process$`__dict__`[[model_name]]
  if (is.null(model_class)) model_class <- sklearn$svm$`__dict__`[[model_name]]
  if (is.null(model_class)) model_class <- sklearn$tree$`__dict__`[[model_name]]
  if (is.null(model_class)) model_class <- sklearn$neighbors$`__dict__`[[model_name]]
  if (is.null(model_class)) model_class <- sklearn$neural_network$`__dict__`[[model_name]]
  if (is.null(model_class)) model_class <- sklearn$random_projection$`__dict__`[[model_name]]
  if (is.null(model_class)) model_class <- sklearn$ensemble$`__dict__`[[model_name]]
  if (is.null(model_class)) model_class <- sklearn$decomposition$`__dict__`[[model_name]]
  if (is.null(model_class)) model_class <- sklearn$semi_supervised$`__dict__`[[model_name]]
  
  if (is.null(model_class)) {
    stop(sprintf("Model '%s' not found in common scikit-learn modules. Please check the model name.", model_name))
  }
  
  # Create and fit the model
  model <- do.call(model_class, params)
  model$fit(x, y, ...)
  
  set.seed(seed)
  
  if (!calibration) {    
    # Get in-sample predictions and residuals
    y_pred <- model$predict(x)
    residuals <- y - y_pred
    df_residual <- length(y) - ncol(x) - 1
  } else {
    # Split data into training and calibration sets
    set.seed(seed)
    train_idx <- sample(nrow(x), size = floor(0.5 * nrow(x)))
    x_train <- x[train_idx, ]
    x_cal <- x[-train_idx, ]
    y_train <- y[train_idx]
    y_cal <- y[-train_idx]
    
    # Convert training data to numpy arrays
    #x_train_np <- reticulate::array_reshape(x_train, c(nrow(x_train), ncol(x_train)))
    #y_train_np <- reticulate::array_reshape(y_train, c(length(y_train), 1))
    
    # Train model on training set
    model$fit(x_train, y_train, ...)
    
    # Get calibration predictions and residuals
    #x_cal_np <- reticulate::array_reshape(x_cal, c(nrow(x_cal), ncol(x_cal)))
    y_cal_pred <- model$predict(x_cal)
    residuals <- y_cal - y_cal_pred
    df_residual <- length(y_cal) - ncol(x_cal) - 1
  }
  
  # Return regressor object
  structure(
    list(
      model = model,
      residuals = residuals,
      df.residual = df_residual
    ),
    class = "regressor"
  )
}

#' Predict method for regressor objects 
#' 
#' @param object A regressor object
#' @param newdata New data to predict on
#' @param nsim Number of simulations for bootstrap/tsbootstrap
#' @param level Confidence level for prediction intervals
#' @param method Method for computing prediction intervals
#' @param seed Seed for the random number generator
#' @param ... Additional arguments
#' @export
#' @method predict regressor
predict.regressor <- function(object, newdata, nsim = 250L, level = 95,
                              method = c("none", "splitconformal", "surrogate", 
                                         "bootstrap", "tsbootstrap", "bayesian"),
                              seed = 123, ...) {
  
  method <- match.arg(method)
  
  if (method == "bayesian") {
    pred <- as.vector(object$model$predict(newdata, return_std=TRUE))
    y_mean <- as.vector(pred[[1]])
    y_std <- as.vector(pred[[2]])
    multiplier <- stats::qnorm(1 - (100 - level)/200)
    lower <- y_mean - multiplier*y_std
    upper <- y_mean + multiplier*y_std
    return(cbind(fit = drop(y_mean), lwr = lower, upr = upper))
  }
  
  # Convert newdata to numpy array
  #newdata_np <- reticulate::array_reshape(newdata, c(nrow(newdata), ncol(newdata)))
  pred <- as.vector(object$model$predict(newdata))
  
  if (method == "none") {
    return(pred)
  }
  
  # Validate method
  if (!(method %in% c("splitconformal", "surrogate", "bootstrap", "tsbootstrap"))) {
    stop("Invalid method. Choose from: 'none', 'splitconformal', 'surrogate', 'bootstrap', 'tsbootstrap'.")
  }
  
  set.seed(seed)
  
  if (method == "bootstrap") {
    errors <- matrix(sample(object$residuals, size = length(pred) * nsim, replace = TRUE), 
                     nrow = length(pred), ncol = nsim)    
  } else if (method == "tsbootstrap") {
    errors <- tseries::tsbootstrap(sample(object$residuals, size = length(pred), replace = TRUE), 
                                   type = "block", nb = nsim)
  } else if (method == "surrogate") {
    errors <- tseries::surrogate(sample(object$residuals, size = length(pred), replace = TRUE), ns = nsim)
  } else { # splitconformal
    multiplier <- quantile(abs(object$residuals), probs = level/100)
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

#' Simulate method for regressor objects
#' 
#' @param object A regressor object
#' @param newdata New data to predict on
#' @param nsim Number of simulations for bootstrap/tsbootstrap
#' @param level Confidence level for prediction intervals
#' @param method Method for computing prediction intervals
#' @param seed Seed for the random number generator
#' @param ... Additional arguments
#' @export
#' @method simulate regressor
simulate.regressor <- function(object, newdata, nsim = 250L, level = 95,
                               method = c("surrogate", "bootstrap", 
                                          "tsbootstrap", "bayesian"),
                               seed = 123, 
                               venv_path = "./venv",
                               ...) {
  set.seed(seed)  
  method <- match.arg(method)  
  
  if (method == "bayesian") {
    pred <- as.vector(object$model$predict(newdata, return_std=TRUE))
    y_mean <- as.vector(pred$y_mean)
    y_std <- as.vector(pred$y_std)
    multiplier <- stats::qnorm(1 - (100 - level)/200)
    lower <- y_mean - multiplier*y_std
    upper <- y_mean + multiplier*y_std
  }
  
  # Convert newdata to numpy array and get predictions
  #newdata_np <- reticulate::array_reshape(newdata, c(nrow(newdata), ncol(newdata)))
  pred <- as.vector(object$model$predict(newdata))
  
  if (method == "bootstrap") {
    errors <- matrix(sample(object$residuals, size = length(pred) * nsim, replace = TRUE), 
                     nrow = length(pred), ncol = nsim)    
  } else if (method == "tsbootstrap") {
    errors <- tseries::tsbootstrap(sample(object$residuals, size = length(pred), replace = TRUE), type = "block", nb = nsim)
  } else if (method == "surrogate") {
    errors <- tseries::surrogate(sample(object$residuals, size = length(pred), replace = TRUE), ns = nsim)
  }
  
  # Simulated predictions
  result <- errors + drop(pred)
  
  # Convert to data.frame to match simulate.lm output format
  result <- as.data.frame(result)
  colnames(result) <- paste0("sim_", seq_len(nsim))
  return(result)
}