#' Fit a boosting model with neural network feature transformation
#' 
#' @param x Input matrix
#' @param y Target vector
#' @param model_name Name of the base model
#' @param n_estimators Number of boosting iterations
#' @param learning_rate Learning rate for boosting
#' @param calibration Whether to calibrate the model
#' @param seed Random seed
#' @param ... Additional arguments passed to the regressor
#' 
#' @export
booster <- function(x, y, model_name, 
                    n_estimators = 100L, 
                    learning_rate = 0.1,
                    tolerance = 1e-4,
                    calibration = FALSE, 
                    seed = 123L, 
                    show_progress = TRUE,
                    verbose = FALSE,
                    ...) {    
    e <- y # residuals
    estimators <- vector("list", n_estimators)
    if (show_progress) {
        pb <- utils::txtProgressBar(min = 0, max = n_estimators, style = 3)
    }
    current_loss <- Inf
    losses <- numeric(n_estimators)
    for (i in seq_len(n_estimators)) {
        # Fit base learner
        model <- regressor(x, y, model_name, calibration = calibration, 
                           seed = seed+i*100, ...)
        # Get predictions
        y_pred <- predict(model, x)
        estimators[[i]] <- model
        # Update residuals
        e <- e - learning_rate * y_pred
        current_loss <- sqrt(mean(e^2))
        if (verbose) {
            cat(sprintf("Iteration %d: loss = %f\n", i, current_loss))
        }
        losses[i] <- current_loss
        if ((i > 2 && (abs(diff(losses[(i-1):i])) < tolerance)) || (i > 2 && diff(losses[(i-1):i]) > 0)) {
            estimators <- estimators[1:(i-1)]
            break
        }
        if (show_progress) {
            utils::setTxtProgressBar(pb, i)
        }
    }
    if (show_progress) {
        close(pb)
    }
    out <- list(estimators = estimators, learning_rate = learning_rate,
                losses = losses, n_estimators = i, show_progress = show_progress)
    return(structure(out, class = "booster"))
}

#' Predict using a boosted model
#' 
#' @param object A boosted model object
#' @param newdata New data to predict on
#' @param ... Additional arguments
#' @export
predict.booster <- function(object, newdata, ...) {
    y_pred <- numeric(nrow(newdata))
    n_estimators <- length(object$estimators)
    # Keep only non-empty elements
    object$estimators <- object$estimators[!sapply(object$estimators, is.null)]
    n_estimators <- length(object$estimators)
    if (object$show_progress) {
        pb <- utils::txtProgressBar(min = 0, max = n_estimators, style = 3)
    }
    for (i in seq_len(n_estimators)) {
        y_pred <- y_pred + object$learning_rate * predict(object$estimators[[i]], newdata)        
        if (object$show_progress) {
            utils::setTxtProgressBar(pb, i)
        }
    }
    if (object$show_progress) {
        close(pb)
    }
    return(y_pred)
}   