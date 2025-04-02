scipy <- NULL
numpy <- NULL
sklearn <- NULL

.onLoad <- function(libname, pkgname) {
    # Check if conda is available
    if (!reticulate::conda_available()) {
        warning("Conda not found. Please install Anaconda or Miniconda to use this package.")
        return(invisible(NULL))
    }
    
    tryCatch({
        # Explicitly use r-reticulate environment
        reticulate::use_condaenv("r-reticulate", required = TRUE)
        
        # Get exact path to r-reticulate environment
        conda_path <- reticulate::conda_list()["r-reticulate", "python"]

        # Install required packages in r-reticulate environment
        reticulate::py_install(c("scipy", "numpy", "scikit-learn"), 
                              envname = "r-reticulate")

        # Import packages from r-reticulate environment directly
        scipy <<- reticulate::import_from_path("scipy", 
                                             path = dirname(conda_path),
                                             delay_load = TRUE)
        numpy <<- reticulate::import_from_path("numpy", 
                                            path = dirname(conda_path),
                                            delay_load = TRUE)
        sklearn <<- reticulate::import_from_path("sklearn", 
                                             path = dirname(conda_path),
                                             delay_load = TRUE)
    }, error = function(e) {
        warning("Error loading Python dependencies: ", e$message)
        return(invisible(NULL))
    })
}