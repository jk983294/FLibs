#' generate test data
#'
#' @param ins_num ukey number
#' @param num how many data pointers
#'
#' @import data.table
#' @export
gen_test_data <- function(ins_num = 2L, num = 1000L) {
    rbindlist(lapply(seq(ins_num), function(ukey_) {
        dt <- as.data.table(FM::GBM_ohlc(ukey = ukey_, start_price = runif(1, 50, 100), n = num))
        dt <- cbind(dt, f0 = runif(num))
        dt[, f1 := close / shift(close, 2L) - 1.]
        dt
    }))
}


#' cal_half_life
#'
#' @param data dt
#' @param cols colums
#' @param max_n max autopcor lag
#'
#' @import data.table
#' @export
cal_half_life <- function(data, cols, max_n = 100) {
    lags <- c(1:5, seq(10, 100, 5), seq(120, 1000, 20), seq(1000, 10000, 100))
    lags <- lags[lags <= max_n]
    dtap <- data[, FM::autopcor(.SD, lags), .SDcols = cols]
    dtap[, `:=`(lag, lags)]
    dtap <- melt(dtap, measure.vars = cols, variable.name = "feature", value.name = "pcor")
    setkey(dtap, feature, lag)
    dtap[lag == max(lags), `:=`(pcor, 0)]
    dthl <- dtap[pcor < 0.5, first(.SD), by = feature]
    setnames(dthl, "lag", "hl")
    dthl
}

#' acf
#'
#' @param data data.table
#' @param cols selected columns
#' @param lags lag windows
#'
#' @import data.table
#' @export
acf <- function(data, cols, lags) {
    if (missing(lags)) {
        lags <- c(1L:5L, 10L, 15L)
    }
    if (length(cols) > 1) {
        res <- rbindlist(lapply(cols, acf, data = data, lags = lags))
        return(res)
    }
    res <- rbindlist(lapply(lags, function(lag) {
        data[, .(col = cols[1], lag = lag, auto_cor = FM::pcor(.SD[[1]], shift(.SD[[1]], lag))), by = .(ukey), .SDcols = cols[1]]
    }))
    res <- res[, .(auto_cor = mean(auto_cor, na.rm = TRUE)), keyby = .(col, lag)]
    res
}

#' compare
#'
#' @param x numeric vector
#' @param y numeric vector
#'
#' @import data.table
#' @export
compare <- function(x, y, digit = 4) {
    stopifnot(length(x) == length(y) & is.atomic(x) & is.atomic(y))
    total_num <- length(x)
    data.table(
        total_num = total_num,
        pcor = FM::pcor(x, y),
        rcor = FM::rcor(x, y),
        equal_ratio = mean(round(x, digit) == round(y, digit), na.rm = TRUE),
        left_na = na_ratio(x),
        right_na = na_ratio(y)
    )
}

#' compare
#'
#' @param x dt
#' @param y dt
#' @param key by vec
#'
#' @import data.table
#' @export
compare_data <- function(left_data, right_data, k = c("ukey", "DataDate", "ticktime")) {
    res <- list()
    left_right_cols_diff <- setdiff(colnames(left_data), colnames(right_data))
    right_left_cols_diff <- setdiff(colnames(right_data), colnames(left_data))
    common_cols <- intersect(colnames(right_data), colnames(left_data))
    common_cols <- common_cols[!(common_cols %in% k)]
    left_nrow <- nrow(left_data)
    right_nrow <- nrow(right_data)
    check <- merge(left_data, right_data, by = k, all = TRUE)
    gc()
    compare <- rbindlist(lapply(common_cols, function(col) {
        x <- check[[paste0(col, ".x")]]
        y <- check[[paste0(col, ".y")]]
        res <- compare(x, y)
        res[, `:=`(col, col)]
        res
    }))
    compare <- compare[order(pcor), .(col, total_num, pcor, rcor, equal_ratio, left_na, right_na)]
    search <- paste0(compare[pcor < 0.995, col], collapse = "|")
    res <- structure(list(
        left_right_cols_diff = left_right_cols_diff,
        right_left_cols_diff = right_left_cols_diff, common_cols = common_cols,
        left_nrow = left_nrow, right_nrow = right_nrow, compare = compare,
        search = search
    ), class = "compare_data")
    res
}

#' cor_mat
#' @description Correlation Matrix
#' @param data dt
#' @param cols cols
#' @param sample_num sample_num
#'
#' @import data.table
#' @export
cor_mat <- function(data, cols, sample_num = 1000000L) {
    sdt <- data[sample(sample_num), .SD, .SDcols = cols]
    cor_mat <- cor(sdt, use = "complete.obs")
    class(cor_mat) <- c("matrix", "cor_mat")
    cor_mat
}

#' pair_cor
#' @description Pairwise Correlation Table
#' @param cor_mat correlation matrix
#'
#' @import data.table
#' @export
pair_cor <- function(cor_mat) {
    pair_cor <- reshape2::melt(cor_mat, value.name = "cor", variable.factor = FALSE, value.factor = FALSE)
    setDT(pair_cor)
    pair_cor[, `:=`(col_1, as.character(Var1))]
    pair_cor[, `:=`(col_2, as.character(Var2))]
    pair_cor[, `:=`(Var1, NULL)]
    pair_cor[, `:=`(Var2, NULL)]
    pair_cor <- pair_cor[col_1 != col_2]
    pair_cor <- pair_cor[order(-abs(cor))]
    setcolorder(pair_cor, c("col_1", "col_2", "cor"))
    pair_cor
}

#' stats
#' @param data dt
#' @param cols cols
#'
#' @import data.table
#' @export
stats <- function(data, cols) {
    ops <- options(warn = -1)
    on.exit(options(ops))
    if (length(cols) > 1) {
        res <- rbindlist(lapply(cols, stats, data = data))
        return(res)
    }
    x <- data[[cols]]
    res <- data.table(
        x = cols,
        n = length(x),
        na_ratio = FM::fm_nan_ratio(x),
        zero_ratio = FM::fm_zero_ratio(x),
        mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        skewness = FM::fm_skew(x),
        kurtosis = FM::fm_kurt(x),
        min = min(x, na.rm = TRUE),
        q005 = quantile(x, 0.005, na.rm = TRUE),
        q50 = quantile(x, 0.5, na.rm = TRUE),
        q995 = quantile(x, 0.995, na.rm = TRUE),
        max = max(x, na.rm = TRUE)
    )
    res[, `:=`(min_ratio, abs(min) / abs(q005))]
    res[, `:=`(max_ratio, abs(max) / abs(q995))]
    res$outlier_ratio <- sum((x > res$mean + 3 * res$sd) | (x < res$mean - 3 * res$sd)) / res$n
    res
    return(res)
}

#' cluster_select
#'
#' @param summaries summaries
#' @param data data.table
#' @param target_num target_num
#' @param sample_num sample_num
#' @param abs_cor_mat abs_cor_mat
#' @param method method
#'
#' @import data.table
#' @export
cluster_select <- function(summaries, data, target_num, sample_num = 500000L,
                           abs_cor_mat = TRUE, method = "ward.D2") {
    stop("Not implemented. do not use summaries (contains information of y)")
    cor_mat <- cor_mat(data, xs = unique(summaries$x), sample_num = sample_num)
    if (abs_cor_mat) {
        cor_mat <- abs(cor_mat)
    }
    dist_mat <- as.dist(1 - cor_mat)
    cluster <- hclust(dist_mat, method = method)
    cut_tree <- cutree(cluster, k = target_num)
    cut_label <- data.table(x = names(cut_tree), cluster = cut_tree)
    grouped <- merge(summaries, cut_label, by = "x", all.x = TRUE)
    selected <- grouped[, .SD[abs(pcor_score) == max(abs(pcor_score))], keyby = .(y, cluster)]
    pair_cor <- pair_cor(cor_mat, plot = FALSE)
    pair_cor <- merge(pair_cor, cut_label[, .(x = x, cluster_1 = cluster)], by = "x", all.x = TRUE)
    pair_cor <- merge(pair_cor, cut_label[, .(y = x, cluster_2 = cluster)], by = "y", all.x = TRUE)
    cluster_stats <- pair_cor[cluster_1 == cluster_2, describe(cor), keyby = cluster_1]
    selected_pair_cor <- pair_cor[x %in% unique(selected$x) & y %in% unique(selected$x)]
    list(
        grouped = grouped, selected = selected, pair_cor = pair_cor,
        selected_pair_cor = selected_pair_cor, cluster_stats = cluster_stats,
        cluster_model = cluster
    )
}

#' elimination_select
#' @description elimination_select
#' @param pair_cor cor
#' @param all_cols cols
#' @param threshold threshold
#'
#' @import data.table progress
#' @export
elimination_select <- function(pair_cor, all_cols, threshold = 0.9) {
    pair_cor[, `:=`(abs_cor, abs(cor))]
    pair_cor <- pair_cor[(x %in% all_cols) & (y %in% all_cols)]
    remained_cols <- all_cols
    removed_cols <- c()
    pair_cor_eliminated <- copy(pair_cor)
    setkey(pair_cor_eliminated, x, y)
    bar <- progress_bar$new(total = length(all_cols))
    for (col in all_cols) {
        if (col %in% removed_cols) {
            next
        }
        related_cols <- c(col, pair_cor_eliminated[x == col &
            abs_cor > threshold, y])
        removed_cols <- c(removed_cols, setdiff(
            related_cols,
            col
        ))
        remained_cols <- setdiff(remained_cols, removed_cols)
        pair_cor_eliminated <- pair_cor_eliminated[!(x %in% removed_cols |
            y %in% removed_cols)]
        bar$tick()
    }
    res <- structure(
        list(
            all_cols = all_cols, threshold = threshold,
            remained_cols = sort(remained_cols), removed_cols = sort(removed_cols),
            pair_cor = pair_cor, pair_cor_eliminated = pair_cor_eliminated
        ),
        class = "elimination_select"
    )
    res
}


#' elimination_select2
#' @description elimination_select2
#' @param pair_cor cor
#' @param all_cols cols
#' @param threshold threshold
#'
#' @import data.table progress
#' @export
elimination_select2 <- function(pair_cor, all_cols, threshold = 0.9) {
    pc <- copy(pair_cor)
    pc2 <- copy(pair_cor)
    setnames(pc2, 1:2, c("y", "x"))
    pc <- rbind(pc, pc2)
    pc[, `:=`(abs_cor, nafill(abs(cor), fill = 1))]
    setkey(pc, x, y)
    remained_cols <- all_cols[[1]]
    bar <- progress_bar$new(total = length(all_cols) - 1)
    for (col in all_cols[-1]) {
        if (pc[.(col, remained_cols), max(abs_cor)] < threshold) {
            remained_cols <- c(remained_cols, col)
        }
        bar$tick()
    }
    remained_cols
}



#' elimination_select3
#' @description elimination_select3
#' @param pair_cor cor
#' @param all_cols cols
#' @param threshold threshold
#'
#' @import data.table progress
#' @export
elimination_select3 <- function(data, all_cols, threshold = 0.9, threads = 1L) {
    remained_cols <- all_cols[[1]]
    bar <- progress_bar$new(total = length(all_cols) - 1)
    for (col in all_cols[-1]) {
        cors <- eval(substitute(
            data[, pcor(.SD, col), .SDcols = remained_cols],
            list(col = as.symbol(col))
        ))
        max_abs_cors <- max(abs(as.numeric(cors)), na.rm = TRUE)
        if (!is.finite(max_abs_cors)) {
            next
        }
        if (max_abs_cors < threshold) {
            remained_cols <- c(remained_cols, col)
        }
        bar$tick()
    }
    remained_cols
}

#' mean pcor by key
#'
#' @param data dt
#' @param f1 column1
#' @param f2 column2
#' @param by_key by_key
#'
#' @import data.table FM
#' @export
group_mean_pcor <- function(dt, f1, f2, by_key) {
    cols <- c(f1, f2)
    data <- dt[, FM::pcor(.SD[[1]], .SD[[2]]), by = by_key, .SDcols = cols]
    names(data) <- c("ukey", "pcor_ukey")
    data[, mean(pcor_ukey, na.rm = TRUE)]
}
