#' factor_turnovers
#' analyzing the turnover properties of a factor
#'
#' @param dt dt, expect factor data
#' @param qtl_name default "f_qtl"
#' @param shifts period array default 1
#'
#' @import data.table
#' @export
factor_turnovers <- function(dt, qtl_name = "f_qtl", shifts = c(1L)) {
    dt <- rbindlist(lapply(shifts, function(p) {
        dt1 <- setDT(.Call(`_FF_calc_qtl_turnover`, dt, qtl_name, p))
        dt1[, shift := p]
        return(dt1)
    }))
    return(dt)
}

#' factor_rank_acf
#' Computes autocorrelation of factor ranks
#'
#' @param dt dt, expect factor data
#' @param f_name default "factor"
#' @param shifts period default 1
#'
#' @import data.table
#' @export
factor_rank_acf <- function(dt, f_name = "factor", shifts = c(1L)) {
    dt <- rbindlist(lapply(shifts, function(p) {
        dt1 <- setDT(.Call(`_FF_calc_rank_acf`, dt, f_name, p))
        dt1[, shift := p]
        return(dt1)
    }))
    return(dt)
}
