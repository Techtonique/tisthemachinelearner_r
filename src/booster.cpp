#include <Rcpp.h>
#include <cmath>
#include <vector>
using namespace Rcpp;

// [[Rcpp::export]]
List boosterCpp(NumericMatrix x, 
                NumericVector y,
                String model_name,
                int n_estimators = 100,
                double learning_rate = 0.1,
                double tolerance = 1e-4,
                bool calibration = false,
                int seed = 123,
                bool show_progress = true,
                bool verbose = false) {
    
    Rcout << "Debug: Starting function\n";
    
    // Initialize residuals
    NumericVector e = clone(y);
    List estimators;  // Don't pre-allocate size, we'll push_back instead
    
    Rcout << "Debug: Setting up progress bar\n";
    // Progress bar setup through R
    Function txtProgressBar("txtProgressBar", Environment::namespace_env("utils"));
    Function setTxtProgressBar("setTxtProgressBar", Environment::namespace_env("utils"));
    SEXP pb;
    if (show_progress) {
        pb = txtProgressBar(_["min"] = 0, _["max"] = n_estimators, _["style"] = 3);
        // Force evaluation of the progress bar
        pb = Rcpp::as<SEXP>(pb);
    }
    
    Rcout << "Debug: Getting R functions\n";
    // Get reference to R's regressor function through the package namespace
    Environment pkg = Environment::namespace_env("tisthemachinelearner");
    Rcout << "Debug: Got package environment\n";
    
    if (!pkg.exists("regressor")) {
        stop("Error: 'regressor' function not found in tisthemachinelearner package");
    }
    Function regressor = pkg["regressor"];
    Rcout << "Debug: Got regressor function\n";
    
    // predict can be accessed from base R
    Function predict("predict");
    Rcout << "Debug: Got predict function\n";
    
    double current_loss = R_PosInf;
    NumericVector losses(n_estimators);
    int final_n_estimators = n_estimators;
    
    for (int i = 0; i < n_estimators; i++) {
        // Fit base learner
        List model = regressor(x, e,  // Note: using residuals 'e' instead of 'y'
                             model_name, 
                             _["calibration"] = calibration,
                             _["seed"] = seed + i * 100);
        
        // Get predictions
        NumericVector y_pred = predict(model, x);
        estimators.push_back(model);  // Use push_back instead of indexing
        
        // Update residuals
        e = e - learning_rate * y_pred;
        
        // Calculate loss
        double sum_squared = 0.0;
        for (int j = 0; j < e.length(); j++) {
            sum_squared += e[j] * e[j];
        }
        current_loss = std::sqrt(sum_squared / e.length());
        
        if (verbose) {
            Rcout << "Iteration " << i + 1 << ": loss = " << current_loss << "\n";
        }
        
        losses[i] = current_loss;
        
        // Early stopping check
        if (i > 1) {
            double loss_diff = std::abs(losses[i] - losses[i-1]);
            if (loss_diff < tolerance || losses[i] > losses[i-1]) {
                final_n_estimators = i + 1;
                break;
            }
        }
        
        if (show_progress) {
            try {
                setTxtProgressBar(pb, i + 1);
            } catch(...) {
                Rcout << "Warning: Could not update progress bar\n";
            }
        }
    }
    
    // Close progress bar if it was created
    if (show_progress) {
        Function close("close");
        close(pb);
    }
    
    // Create and return output list
    List out = List::create(
        _["estimators"] = estimators,
        _["learning_rate"] = learning_rate,
        _["losses"] = losses,
        _["n_estimators"] = final_n_estimators,
        _["show_progress"] = show_progress
    );
    out.attr("class") = "booster";
    
    return out;
}

// Add this new prediction function
// [[Rcpp::export]]
NumericVector predictBoosterCpp(List booster, NumericMatrix x) {
    List estimators = booster["estimators"];
    double learning_rate = booster["learning_rate"];
    int n_estimators = estimators.length();
    
    // Initialize predictions with zeros
    NumericVector predictions(x.nrow());
    Function predict("predict");
    
    // Accumulate predictions from all estimators
    for (int i = 0; i < n_estimators; i++) {
        NumericVector current_pred = predict(estimators[i], x);
        predictions = predictions + learning_rate * current_pred;
    }
    
    return predictions;
} 