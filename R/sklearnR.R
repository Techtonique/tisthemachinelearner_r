#' Train a machine learning model
#' 
#' @param X Feature matrix
#' @param y Target vector
#' @param model_name Name of the sklearn model to use (e.g., "RandomForestRegressor")
#' @param params List of parameters for the model
#' @return A model ID string (path to saved model)
#' @export
train_model <- function(X, y, model_name, params = list()) {
  return(jsonlite::fromJSON(paste(system2("python", 
                    args = system.file("python", "tml_script.py", 
                                       package = "tisthemachinelearner"), 
                    input = jsonlite::toJSON(list(
                                  action = "train",
                                  X = as.matrix(X),
                                  y = as.numeric(y),
                                  model_name = model_name,
                                  params = params
                                ), auto_unbox = TRUE), 
                    stdout = TRUE), collapse = ""))$model_id)
}

#' Make predictions using a trained model
#' 
#' @param X_new New feature matrix for predictions
#' @param model_id Model ID (path) returned by train_model
#' @return Vector of predictions
#' @export
predict_model <- function(X_new, model_id) {
  return(jsonlite::fromJSON(paste(system2("python", 
                    args = system.file("python", "tml_script.py", package = "tisthemachinelearner"), 
                    input = jsonlite::toJSON(list(
                            action = "predict",
                            X_new = as.matrix(X_new),
                            model_id = model_id
                          ), auto_unbox = TRUE), 
                    stdout = TRUE), collapse = ""))$predictions)
}
