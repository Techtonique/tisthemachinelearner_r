scipy <- NULL
numpy <- NULL
sklearn <- NULL

.onLoad <- function(libname, pkgname) {
    reticulate::py_install("scipy")
    reticulate::py_install("numpy")
    reticulate::py_install("scikit-learn")

    reticulate::py_require("scipy")
    reticulate::py_require("numpy")
    reticulate::py_require("scikit-learn")

  # delay load foo module (will only be loaded when accessed via $)
  scipy <<- reticulate::import("scipy", delay_load = TRUE)
  numpy <<- reticulate::import("numpy", delay_load = TRUE)
  sklearn <<- reticulate::import("sklearn", delay_load = TRUE)
}