#' get all drawdown periods
#'
#' @param x numeric vector
#'
#' @import data.table
#' @export
get_all_drawdown <- function(x) {
    dt <- data.table(id = seq_along(x), x = x)
    dt[, `:=`(dd, cummax(x) - x)]
    dt[, `:=`(group, cumsum(dd == 0))]
    group_dt <- dt[dd > 0, .(
        from = min(id), trough = id[which.max(dd)],
        to = max(id), depth = max(dd), length = .N, peak_to_trough = which.max(dd),
        recovery = .N - which.max(dd)
    ), keyby = group]
    group_dt[, `:=`(group, seq_len(.N))]
    setkeyv(group_dt, "group")
    group_dt[TRUE]
}
