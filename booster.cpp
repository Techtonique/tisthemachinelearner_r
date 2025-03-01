#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List boosterCpp(NumericMatrix x, NumericVector y, std::string model_name,
                int n_estimators = 100, double learning_rate = 0.1, double tolerance = 1e-4,
                bool calibration = false, int seed = 123, bool show_progress = true, bool verbose = false) {
  NumericVector e = clone(y); // Residuals
  List estimators(n_estimators);
  NumericVector losses(n_estimators);
  double current_loss = R_PosInf;

  Function setTxtProgressBar("setTxtProgressBar"), txtProgressBar("txtProgressBar"), close("close");
  SEXP pb = R_NilValue;
  if (show_progress) {
    pb = txtProgressBar(_["min"] = 0, _["max"] = n_estimators, _["style"] = 3);
  }

  Environment env = Environment::namespace_env("tisthemachinelearner");
  Function regressor = env["regressor"];
  Function predict = env["predict.regressor"];
  Function as_matrix("as.matrix");  // Get as.matrix function
  
  for (int i = 0; i < n_estimators; i++) {
    if (verbose) Rcout << "Starting iteration " << i + 1 << "\n";
    
    try {
      if (verbose) Rcout << "Calling regressor with model_name: " << model_name << "\n";
      
      // Convert x to matrix before passing to regressor
      NumericMatrix x_matrix = as<NumericMatrix>(as_matrix(x));
      
      // Create a list of arguments explicitly
      List args = List::create(
        _["x"] = x_matrix,  // Use converted matrix
        _["y"] = y,
        _["model_name"] = model_name,
        _["calibration"] = calibration,
        _["seed"] = seed + i * 100
      );
      
      if (verbose) Rcout << "Created argument list\n";
      
      // Call regressor with error handling
      Function try_catch("try");
      SEXP result = try_catch(Rcpp::wrap(regressor), args, _["silent"] = false);
      
      if (verbose) Rcout << "Called regressor\n";
      
      // Check if there was an error
      if (Rf_inherits(result, "try-error")) {
        stop("Error in regressor call: %s", as<std::string>(result));
      }
      
      // Convert result to List
      List model(result);
      if (verbose) Rcout << "Successfully created model\n";
      
      if (verbose) Rcout << "Predicting...\n";
      NumericVector y_pred = as<NumericVector>(predict(model, Named("newdata") = x_matrix));
      if (verbose) Rcout << "Got predictions\n";
      
      estimators[i] = model;

      for (int j = 0; j < e.size(); j++) {
        e[j] -= learning_rate * y_pred[j];
      }

      current_loss = sqrt(mean(pow(e, 2)));
      losses[i] = current_loss;

      if (verbose) {
        Rcout << "Iteration " << (i + 1) << ": loss = " << current_loss << "\n";
      }

      if ((i > 2 && std::abs(losses[i] - losses[i - 1]) < tolerance) | 
          (i > 2 && losses[i] > losses[i - 1])) {
        estimators = estimators[Range(0, i - 1)];
        break;
      }

      if (show_progress) {
        setTxtProgressBar(pb, i + 1);
      }
    } catch(const std::exception& e) {
      stop("Error in regressor: %s", e.what());
    }
  }

  if (show_progress) {
    close(pb);
  }

  return List::create(
    _["estimators"] = estimators,
    _["learning_rate"] = learning_rate,
    _["losses"] = losses,
    _["n_estimators"] = estimators.size(),
    _["show_progress"] = show_progress
  );
}
