#' factor_turnovers
#' analyzing the turnover properties of a factor
#'
#' @param dt dt, expect factor data
#' @param qtl_name default "f_qtl"
#' @param shift period default 1
#'
#' @import data.table
#' @export
factor_turnovers <- function(dt, qtl_name = "f_qtl", shift = 1L) {
    dt <- setDT(.Call(`_FF_calc_qtl_turnover`, dt, qtl_name, shift))
    return(dt)
}