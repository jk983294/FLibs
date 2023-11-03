#' Create Model Object from the ornate::Model C++ Class
#'
#' Allows for the creation of a Model Object in _C++_ from _R_
#' using the _C++_ ornate::Model class.
#'
#'
#' @return
#' A `Model` object from the _C++_ ornate::Model Class.
#'
#' @examples
#' ##################
#' ## Constructor
#'
#' # Construct new FT::Model object called "m"
#' m = new(FT::Model)
#'
#' ##################
#' ## Getters
#'
#' m$train()
#'
#' m$fit()
#'
#' @name Model
#' @export Model

# ^^^^^^^^^^^^^^^^
# Export the "Model" C++ class by explicitly requesting ShmData be
# exported via roxygen2's export tag.
# Also, provide a name for the Rd file.


# Load the Rcpp module exposed with RCPP_MODULE( ... ) macro.
Rcpp::loadModule(module = "FT", TRUE)