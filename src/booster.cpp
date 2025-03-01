#include <Rcpp.h>
#include <progress.hpp>
#include <progress_bar.hpp>
#include <cmath>
#include <vector>
using namespace Rcpp;

//' Fit a boosting model with neural network feature transformation
//'
//' @param x Input matrix
//' @param y Target vector
//' @param model_name Name of the base model
//' @param n_estimators Number of boosting iterations
//' @param learning_rate Learning rate for boosting
//' @param tolerance Convergence tolerance
//' @param calibration Whether to calibrate the model
//' @param seed Random seed
//' @param show_progress Whether to show progress bar
//' @param verbose Whether to print detailed output
//'
//' @export
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
        
    // Set random seed
    Environment base("package:base");
    Function set_seed = base["set.seed"];
    set_seed(seed);
    
    // Initialize residuals
    NumericVector e = clone(y);
    List estimators;  // Don't pre-allocate size, we'll push_back instead
    
    // Initialize progress bar
    Progress p(n_estimators, show_progress);
    
    // Get reference to R's regressor function through the package namespace
    Environment pkg = Environment::namespace_env("tisthemachinelearner");
    
    if (!pkg.exists("regressor")) {
        stop("Error: 'regressor' function not found in tisthemachinelearner package");
    }
    Function regressor = pkg["regressor"];
    
    // predict can be accessed from base R
    Function predict("predict");
    
    double current_loss = R_PosInf;
    NumericVector losses(n_estimators);
    int final_n_estimators = n_estimators;
    
    for (int i = 0; i < n_estimators; i++) {
        if (Progress::check_abort())
            break;
            
        // Set a new seed for each iteration
        set_seed(seed + i * 100);
            
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
        
        p.increment();
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

//' Predict using a boosted model
//'
//' @param booster A boosted model object
//' @param x New data to predict on
//'
//' @export
// [[Rcpp::export]]
NumericVector predictBoosterCpp(List booster, NumericMatrix x) {
    List estimators = booster["estimators"];
    double learning_rate = booster["learning_rate"];
    int n_estimators = estimators.length();
    
    // Initialize predictions with zeros
    NumericVector predictions(x.nrow());
    Function predict("predict");
    
    // Initialize progress bar
    Progress p(n_estimators, (bool)booster["show_progress"]);
    
    // Accumulate predictions from all estimators
    for (int i = 0; i < n_estimators; i++) {
        if (Progress::check_abort())
            break;
            
        NumericVector current_pred = predict(estimators[i], x);
        predictions = predictions + learning_rate * current_pred;
        
        p.increment();
    }
    
    return predictions;
} 