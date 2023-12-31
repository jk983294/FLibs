#' expand string
#'
#' @param str str to be expanded
#' @param ... list used to expand
#' @param expand bool, whether to expand ... list
#' @export
str_expand <- function(str, ..., expand = FALSE) {
    if (expand) {
        args <- expand.grid(..., stringsAsFactors = FALSE)
        args <- cbind(.i = seq_len(nrow(args)), args)
    } else {
        args <- list(...)
        args <- c(list(.i = seq_len(max(lengths(args)))), args)
    }
    strs <- .mapply(function(...) {
        glue::glue(str, .envir = list(...), .open = "{", .close = "}")
    }, args, NULL)
    unlist(strs)
}

#' dt_cn_fut_code
#' @description get all future code
#'
#' @import data.table
#' @export
dt_cn_fut_code <- function () {
    l <- .Call(`_FZ_all_cn_fut_code`)
    as.data.table(l)
}