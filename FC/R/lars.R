backsolvet <- function(r, x, k=ncol(r)) {
  backsolve(r,x,k,transpose=TRUE)
}

updateR <- function(xnew, R = NULL, xold, eps = .Machine$double.eps, Gram = FALSE) {
###Gram argument determines the nature of xnew and xold
  xtx <- if(Gram) xnew else sum(xnew^2)
  norm.xnew <- sqrt(xtx)
  if(is.null(R)) {
    R <- matrix(norm.xnew, 1, 1)
    attr(R, "rank") <- 1
    return(R)
  }
  Xtx <- if(Gram) xold else drop(t(xnew) %*% xold)
  r <- backsolvet(R, Xtx)
  rpp <- norm.xnew^2 - sum(r^2)
  rank <- attr(R, "rank")	### check if R is machine singular
  if(rpp <= eps)
    rpp <- eps
  else {
    rpp <- sqrt(rpp)
    rank <- rank + 1
  }
  R <- cbind(rbind(R, 0), c(r, rpp))
  attr(R, "rank") <- rank
  R
}

#' lars_gram_xy
#' @description lars based on gram and xy
#'
#' @param Gram = X^T * X
#' @param Xy_vec = X^T * y
#' @param max.steps how many features get selected
#' @param type lasso
#' @param trace default FALSE
#' @param normalize default TRUE
#' @param intercept default TRUE
#' @param eps default 1e-12
#' @param use.Gram default 1e-12
#'
#' @export
lars_gram_xy <- function(Gram, Xy_vec, max.steps, type = c("lasso", "lar", "forward.stagewise", "stepwise"),
    trace = FALSE, normalize = TRUE, intercept = TRUE,
    eps = 1e-12, use.Gram = TRUE)
{
    call <- match.call()
    type <- match.arg(type)
    TYPE <- switch(type, lasso = "LASSO", lar = "LAR", forward.stagewise = "Forward Stagewise", stepwise = "Forward Stepwise")
    if (trace)
        cat(paste(TYPE, "sequence\n"))
    m <- dim(Gram)[[1]]
    if (missing(max.steps))
        max.steps <- 8 * min(m)
    ignores <- NULL
    beta <- matrix(0, max.steps + 1, m)
    lambda = double(max.steps)
    Gamrat <- NULL
    arc.length <- NULL
    R2 <- 1
    first.in <- integer(m)
    active <- NULL
    im <- inactive <- seq(m)
    actions <- as.list(seq(max.steps))
    drops <- FALSE
    Sign <- NULL
    R <- NULL
    k <- 0
    while ((k < max.steps) & (length(active) < m - length(ignores))) {
        action <- NULL
        C <- Xy_vec[inactive]
        Cmax <- max(abs(C))
        if (Cmax < eps * 100) {
            if (trace)
                cat("Max |corr| = 0; exiting...\n")
            break
        }
        k <- k + 1
        lambda[k] = Cmax
        if (!any(drops)) {
            new <- abs(C) >= Cmax - eps
            C <- C[!new]
            new <- inactive[new]
            for (inew in new) {
                if (use.Gram) {
                  R <- updateR(Gram[inew, inew], R, drop(Gram[inew, active]), Gram = TRUE, eps = eps)
                }
                else {
                  R <- updateR(x[, inew], R, x[, active], Gram = FALSE, eps = eps)
                }
                if (attr(R, "rank") == length(active)) {
                  nR <- seq(length(active))
                  R <- R[nR, nR, drop = FALSE]
                  attr(R, "rank") <- length(active)
                  ignores <- c(ignores, inew)
                  action <- c(action, -inew)
                  if (trace)
                    cat("LARS Step", k, ":\t Variable", inew, "\tcollinear; dropped for good\n")
                }
                else {
                  if (first.in[inew] == 0)
                    first.in[inew] <- k
                  active <- c(active, inew)
                  Sign <- c(Sign, sign(Xy_vec[inew]))
                  action <- c(action, inew)
                  if (trace)
                    cat("LARS Step", k, ":\t Variable", inew, "\tadded\n")
                }
            }
        }
        else action <- -dropid
        Gi1 <- backsolve(R, backsolvet(R, Sign))
        dropouts <- NULL
        if (type == "forward.stagewise") {
            directions <- Gi1 * Sign
            if (!all(directions > 0)) {
                if (use.Gram) {
                  nnls.object <- nnls.lars(active, Sign, R, directions,
                    Gram[active, active], trace = trace, use.Gram = TRUE,
                    eps = eps)
                }
                else {
                  nnls.object <- nnls.lars(active, Sign, R, directions,
                    x[, active], trace = trace, use.Gram = FALSE,
                    eps = eps)
                }
                positive <- nnls.object$positive
                dropouts <- active[-positive]
                action <- c(action, -dropouts)
                active <- nnls.object$active
                Sign <- Sign[positive]
                Gi1 <- nnls.object$beta[positive] * Sign
                R <- nnls.object$R
                C <- Xy_vec[-c(active, ignores)]
            }
        }
        A <- 1/sqrt(sum(Gi1 * Sign))
        w <- A * Gi1
        if ((length(active) >= m - length(ignores)) | type == "stepwise") {
            gamhat <- Cmax/A
        }
        else {
            if (use.Gram) {
                a <- drop(w %*% Gram[active, -c(active, ignores), drop = FALSE])
            }
            gam <- c((Cmax - C)/(A - a), (Cmax + C)/(A + a))
            gamhat <- min(min(gam[gam > eps], na.rm = TRUE), Cmax/A)
        }
        if (type == "lasso") {
            dropid <- NULL
            b1 <- beta[k, active]
            z1 <- -b1/w
            zmin <- min(z1[z1 > eps], gamhat)
            if (zmin < gamhat) {
                gamhat <- zmin
                drops <- z1 == zmin
            }
            else drops <- FALSE
        }
        beta[k + 1, ] <- beta[k, ]
        beta[k + 1, active] <- beta[k + 1, active] + gamhat * w
        if (use.Gram) {
            Xy_vec <- Xy_vec - gamhat * Gram[, active, drop = FALSE] %*% w
        }
        else {
            Xy_vec <- drop(t(residuals) %*% x)
        }
        Gamrat <- c(Gamrat, gamhat/(Cmax/A))
        arc.length <- c(arc.length, gamhat)
        if (type == "lasso" && any(drops)) {
            dropid <- seq(drops)[drops]
            for (id in rev(dropid)) {
                if (trace)
                  cat("Lasso Step", k + 1, ":\t Variable", active[id], "\tdropped\n")
                R <- downdateR(R, id)
            }
            dropid <- active[drops]
            beta[k + 1, dropid] <- 0
            active <- active[!drops]
            Sign <- Sign[!drops]
        }
        actions[[k]] <- action
        inactive <- im[-c(active, ignores)]
        if (type == "stepwise")
            Sign = Sign * 0
    }
    beta <- beta[seq(k + 1), , drop = FALSE]
    lambda = lambda[seq(k)]
    if (trace)
        cat("Computing residuals, RSS etc .....\n")
    actions = actions[seq(k)]
    netdf = sapply(actions, function(x) sum(sign(x)))
    df = cumsum(netdf)
    if (intercept)
        df = c(Intercept = 1, df + 1)
    else df = c(Null = 0, df)
    object <- list(call = call, type = TYPE, df = df, lambda = lambda, beta = beta)
    object
}