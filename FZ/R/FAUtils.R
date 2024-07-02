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

#' read table
#' @description read to dt
#'
#' @import tools data.table arrow fst
#' @export
read <- function(file = "", columns = NULL, file_type = "") {
    ext_ <- tools::file_ext(file)
    dt <- NULL
    if (ext_ == "feather" || file_type == "feather") {
        if (is.null(columns) || length(columns) == 0) {
            dt <- data.table::setDT(arrow::read_feather(file))
        } else {
            dt <- data.table::setDT(arrow::read_feather(file, col_select = columns))
        }
    } else if (ext_ == "fst" || file_type == "fst") {
        if (is.null(columns) || length(columns) == 0) {
            dt <- fst::read_fst(file, as.data.table = TRUE)
        } else {
            dt <- fst::read_fst(file, columns = columns, as.data.table = TRUE)
        }
    }  else if (ext_ == "csv" || file_type == "csv") {
        dt <- data.table::as.data.table(data.table::fread(file))
        if (is.null(columns) || length(columns) == 0) {
        } else {
            dt <- dt[, ..columns]
        }
    } else {
        lines <- readLines(file)
        dt <- cbind(data.table(seq = seq(1:length(a))), data.table(line = lines))
    }
    return(dt)
}