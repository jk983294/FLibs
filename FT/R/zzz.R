.onLoad <- function(libname, pkgname) {
  if (!requireNamespace("FZ", quietly = TRUE)) {
    stop("FZ required")
  }
  suc <- FZ::check_to(0L)
  invisible(NULL)
}