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
        dt <- cbind(data.table(seq = seq(1:length(lines))), data.table(line = lines))
    }
    return(dt)
}

#' read files to table
#' @description read files to dt
#'
#' @import tools data.table arrow fst
#' @export
reads <- function(file_pattern = "", dates = NULL, columns = NULL, file_type = "") {
    dt <- data.table::rbindlist(lapply(dates, function(date_) {
        file_path <- .Call(`_FZ_fz_replace_time_placeholder`, file_pattern, date_)
        dt1 <- FZ::read(file_path, columns = columns, file_type = file_type)
        dt1
    }))
    return(dt)
}

#' write table
#' @description write to file
#'
#' @import tools data.table arrow fst
#' @export
write <- function(dt, file = "", file_type = "") {
    ext_ <- tools::file_ext(file)
    if (ext_ == "feather" || file_type == "feather") {
        arrow::write_feather(dt, file)
    } else if (ext_ == "fst" || file_type == "fst") {
        fst::write.fst(dt, file)
    }  else if (ext_ == "csv" || file_type == "csv") {
        data.table::fwrite(dt, file)
    } else {
        writeLines(text = dt, con = file)
    }
    return(TRUE)
}