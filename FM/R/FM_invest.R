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

#' Longest drawdown measures
#'
#' @param x numeric vector, cumulative PnL with unit capital
#'
#' @export
longest_drawdown <- function (x) {
    dd <- cummax(x) - x
    di <- c(1L, which(dd == 0), length(dd))
    did <- diff(di)
    dim <- max(did)
    dii <- which.max(did)
    dbegin <- di[dii]
    dend <- di[dii + 1L]
    ddx <- dd[dbegin:dend]
    list(depth = max(ddx), trough = trough <- dbegin + which.max(ddx) - 1L,
        from = dbegin, to = dend, length = dim + 1L, peak_to_trough = trough - dbegin + 1L,
        recovery = dend - trough)
}

#' Maximal drawdown measures
#'
#' @param x numeric vector, cumulative PnL with unit capital
#'
#' @export
max_drawdown <- function (x) {
    n <- length(x)
    dd <- cummax(x) - x
    trough <- which.max(dd)
    from <- which.max(cumsum(dd[1:trough] == 0))
    to <- trough + which.min(dd[trough:n]) - 1L
    if (dd[to] > 0)
        to <- n
    list(depth = max(dd), trough = trough, from = from, to = to,
        length = to - from + 1L, peak_to_trough = trough - from + 1L, recovery = to - trough)
}
