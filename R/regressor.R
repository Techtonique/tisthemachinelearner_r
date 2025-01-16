#' Create a regression model using tisthemachinelearner
#' 
#' @param x Feature matrix
#' @param y Target vector
#' @param model_name Name of the sklearn model to use
#' @param ... Additional parameters passed to the sklearn model
#' @return A regressor object
#' @export
regressor <- function(x, y, model_name, ...) {
  # Import tisthemachinelearner
  tml <- reticulate::import("tisthemachinelearner")
  
  # Create model instance
  model <- tml$Regressor(model_name, ...)
  
  # Convert inputs to numpy arrays
  np <- reticulate::import("numpy")
  x_array <- np$array(x)
  y_array <- np$array(y)
  
  # Fit the model
  model$fit(x_array, y_array)
  
  # Return the fitted model
  structure(
    list(
      model = model
    ),
    class = "regressor"
  )
}

#' @export
predict.regressor <- function(object, newdata, ...) {
  np <- reticulate::import("numpy")
  x_array <- np$array(newdata)
  as.vector(object$model$predict(x_array))
}
