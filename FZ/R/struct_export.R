#' Create BizDayConfig Object from the zerg::BizDayConfig C++ Class
#'
#' Allows for the creation of a BizDayConfig Object in _C++_ from _R_
#' using the _C++_ zerg::BizDayConfig class.
#'
#'
#' @return
#' A `BizDayConfig` object from the _C++_ zerg::BizDayConfig Class.
#'
#' @examples
#' ##################
#' ## Constructor
#'
#' # Construct new FZ::BizDayConfig object called "m"
#' m = new(FZ::BizDayConfig)
#'
#'
#' @name BizDayConfig
#' @export BizDayConfig

# ^^^^^^^^^^^^^^^^
# Export the "BizDayConfig" C++ class by explicitly requesting ShmData be
# exported via roxygen2's export tag.
# Also, provide a name for the Rd file.


# Load the Rcpp module exposed with RCPP_MODULE( ... ) macro.
Rcpp::loadModule(module = "FZ", TRUE)