#' calc R2 between
#'
#' @param pred numeric vector
#' @param obs numeric vector
#' @param formula formula
#' @param na.rm default TRUE
#'
#' @export
R2 <- function(pred, obs, formula = "corr", na.rm = TRUE) {
    n <- sum(complete.cases(pred))
    switch(formula, corr = cor(obs, pred, use = ifelse(na.rm, "complete.obs", "everything"))^2,
        traditional = 1 - (sum((obs - pred)^2, na.rm = na.rm)/((n - 1) * var(obs, na.rm = na.rm))))
}