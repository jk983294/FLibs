// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// get_binned_stats
List get_binned_stats(const NumericVector& x, const NumericVector& y, size_t n_bin, int threads);
RcppExport SEXP _FM_get_binned_stats(SEXP xSEXP, SEXP ySEXP, SEXP n_binSEXP, SEXP threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type y(ySEXP);
    Rcpp::traits::input_parameter< size_t >::type n_bin(n_binSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(get_binned_stats(x, y, n_bin, threads));
    return rcpp_result_gen;
END_RCPP
}
// fast_hist_n
List fast_hist_n(const NumericVector& x, size_t bins, double min_val, double max_val, int threads);
RcppExport SEXP _FM_fast_hist_n(SEXP xSEXP, SEXP binsSEXP, SEXP min_valSEXP, SEXP max_valSEXP, SEXP threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< size_t >::type bins(binsSEXP);
    Rcpp::traits::input_parameter< double >::type min_val(min_valSEXP);
    Rcpp::traits::input_parameter< double >::type max_val(max_valSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(fast_hist_n(x, bins, min_val, max_val, threads));
    return rcpp_result_gen;
END_RCPP
}
// fm_get_ret
std::vector<double> fm_get_ret(const std::vector<double>& x, int lag, int skip);
RcppExport SEXP _FM_fm_get_ret(SEXP xSEXP, SEXP lagSEXP, SEXP skipSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type lag(lagSEXP);
    Rcpp::traits::input_parameter< int >::type skip(skipSEXP);
    rcpp_result_gen = Rcpp::wrap(fm_get_ret(x, lag, skip));
    return rcpp_result_gen;
END_RCPP
}
// t0_nav
std::vector<double> t0_nav(const std::vector<double>& signals, const std::vector<double>& ret1, double long_t, double short_t, double cost);
RcppExport SEXP _FM_t0_nav(SEXP signalsSEXP, SEXP ret1SEXP, SEXP long_tSEXP, SEXP short_tSEXP, SEXP costSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type signals(signalsSEXP);
    Rcpp::traits::input_parameter< const std::vector<double>& >::type ret1(ret1SEXP);
    Rcpp::traits::input_parameter< double >::type long_t(long_tSEXP);
    Rcpp::traits::input_parameter< double >::type short_t(short_tSEXP);
    Rcpp::traits::input_parameter< double >::type cost(costSEXP);
    rcpp_result_gen = Rcpp::wrap(t0_nav(signals, ret1, long_t, short_t, cost));
    return rcpp_result_gen;
END_RCPP
}
// factor_nav
std::vector<double> factor_nav(const std::vector<double>& signals, const std::vector<double>& ret1);
RcppExport SEXP _FM_factor_nav(SEXP signalsSEXP, SEXP ret1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type signals(signalsSEXP);
    Rcpp::traits::input_parameter< const std::vector<double>& >::type ret1(ret1SEXP);
    rcpp_result_gen = Rcpp::wrap(factor_nav(signals, ret1));
    return rcpp_result_gen;
END_RCPP
}
// sharpe
double sharpe(const std::vector<double>& rets);
RcppExport SEXP _FM_sharpe(SEXP retsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type rets(retsSEXP);
    rcpp_result_gen = Rcpp::wrap(sharpe(rets));
    return rcpp_result_gen;
END_RCPP
}
// autopcor
List autopcor(const List& dt, const std::vector<int>& lags, int threads);
RcppExport SEXP _FM_autopcor(SEXP dtSEXP, SEXP lagsSEXP, SEXP threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List& >::type dt(dtSEXP);
    Rcpp::traits::input_parameter< const std::vector<int>& >::type lags(lagsSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(autopcor(dt, lags, threads));
    return rcpp_result_gen;
END_RCPP
}
// pcor
SEXP pcor(SEXP x, SEXP y, int x_sign, int y_sign, int threads);
RcppExport SEXP _FM_pcor(SEXP xSEXP, SEXP ySEXP, SEXP x_signSEXP, SEXP y_signSEXP, SEXP threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type x_sign(x_signSEXP);
    Rcpp::traits::input_parameter< int >::type y_sign(y_signSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(pcor(x, y, x_sign, y_sign, threads));
    return rcpp_result_gen;
END_RCPP
}
// rcor
SEXP rcor(SEXP x, SEXP y, int x_sign, int y_sign, int threads);
RcppExport SEXP _FM_rcor(SEXP xSEXP, SEXP ySEXP, SEXP x_signSEXP, SEXP y_signSEXP, SEXP threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type x_sign(x_signSEXP);
    Rcpp::traits::input_parameter< int >::type y_sign(y_signSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcor(x, y, x_sign, y_sign, threads));
    return rcpp_result_gen;
END_RCPP
}
// fast_quantile_kernel
List fast_quantile_kernel(const NumericVector& x, const IntegerVector& group, std::vector<double> qs, int threads);
RcppExport SEXP _FM_fast_quantile_kernel(SEXP xSEXP, SEXP groupSEXP, SEXP qsSEXP, SEXP threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type group(groupSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type qs(qsSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(fast_quantile_kernel(x, group, qs, threads));
    return rcpp_result_gen;
END_RCPP
}
// GBM_ohlc
List GBM_ohlc(int ukey, double start_price, int n, int DataDate);
RcppExport SEXP _FM_GBM_ohlc(SEXP ukeySEXP, SEXP start_priceSEXP, SEXP nSEXP, SEXP DataDateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type ukey(ukeySEXP);
    Rcpp::traits::input_parameter< double >::type start_price(start_priceSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type DataDate(DataDateSEXP);
    rcpp_result_gen = Rcpp::wrap(GBM_ohlc(ukey, start_price, n, DataDate));
    return rcpp_result_gen;
END_RCPP
}
// fm_nan_ratio
double fm_nan_ratio(const std::vector<double>& x);
RcppExport SEXP _FM_fm_nan_ratio(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(fm_nan_ratio(x));
    return rcpp_result_gen;
END_RCPP
}
// fm_inf_ratio
double fm_inf_ratio(const std::vector<double>& x);
RcppExport SEXP _FM_fm_inf_ratio(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(fm_inf_ratio(x));
    return rcpp_result_gen;
END_RCPP
}
// fm_zero_ratio
double fm_zero_ratio(SEXP x, double precision);
RcppExport SEXP _FM_fm_zero_ratio(SEXP xSEXP, SEXP precisionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type precision(precisionSEXP);
    rcpp_result_gen = Rcpp::wrap(fm_zero_ratio(x, precision));
    return rcpp_result_gen;
END_RCPP
}
// fm_skew
double fm_skew(const std::vector<double>& x);
RcppExport SEXP _FM_fm_skew(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(fm_skew(x));
    return rcpp_result_gen;
END_RCPP
}
// fm_kurt
double fm_kurt(const std::vector<double>& x);
RcppExport SEXP _FM_fm_kurt(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(fm_kurt(x));
    return rcpp_result_gen;
END_RCPP
}
// fcap
SEXP fcap(SEXP x, double lower, double upper);
RcppExport SEXP _FM_fcap(SEXP xSEXP, SEXP lowerSEXP, SEXP upperSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type lower(lowerSEXP);
    Rcpp::traits::input_parameter< double >::type upper(upperSEXP);
    rcpp_result_gen = Rcpp::wrap(fcap(x, lower, upper));
    return rcpp_result_gen;
END_RCPP
}
// log_trim
std::vector<double> log_trim(const std::vector<double>& x);
RcppExport SEXP _FM_log_trim(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(log_trim(x));
    return rcpp_result_gen;
END_RCPP
}
// cb_rt
std::vector<double> cb_rt(const std::vector<double>& x);
RcppExport SEXP _FM_cb_rt(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cb_rt(x));
    return rcpp_result_gen;
END_RCPP
}
// sign_pow
std::vector<double> sign_pow(const std::vector<double>& x, double exp);
RcppExport SEXP _FM_sign_pow(SEXP xSEXP, SEXP expSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type exp(expSEXP);
    rcpp_result_gen = Rcpp::wrap(sign_pow(x, exp));
    return rcpp_result_gen;
END_RCPP
}
// fm_all_equal
bool fm_all_equal(const std::vector<double>& x, double tol, bool na_rm);
RcppExport SEXP _FM_fm_all_equal(SEXP xSEXP, SEXP tolSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(fm_all_equal(x, tol, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// vec_scale
NumericVector vec_scale(const NumericVector& x, double mean, double sd);
RcppExport SEXP _FM_vec_scale(SEXP xSEXP, SEXP meanSEXP, SEXP sdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type mean(meanSEXP);
    Rcpp::traits::input_parameter< double >::type sd(sdSEXP);
    rcpp_result_gen = Rcpp::wrap(vec_scale(x, mean, sd));
    return rcpp_result_gen;
END_RCPP
}
// vec_scale_xy
NumericVector vec_scale_xy(const NumericVector& x, const NumericVector& y);
RcppExport SEXP _FM_vec_scale_xy(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(vec_scale_xy(x, y));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_FM_get_binned_stats", (DL_FUNC) &_FM_get_binned_stats, 4},
    {"_FM_fast_hist_n", (DL_FUNC) &_FM_fast_hist_n, 5},
    {"_FM_fm_get_ret", (DL_FUNC) &_FM_fm_get_ret, 3},
    {"_FM_t0_nav", (DL_FUNC) &_FM_t0_nav, 5},
    {"_FM_factor_nav", (DL_FUNC) &_FM_factor_nav, 2},
    {"_FM_sharpe", (DL_FUNC) &_FM_sharpe, 1},
    {"_FM_autopcor", (DL_FUNC) &_FM_autopcor, 3},
    {"_FM_pcor", (DL_FUNC) &_FM_pcor, 5},
    {"_FM_rcor", (DL_FUNC) &_FM_rcor, 5},
    {"_FM_fast_quantile_kernel", (DL_FUNC) &_FM_fast_quantile_kernel, 4},
    {"_FM_GBM_ohlc", (DL_FUNC) &_FM_GBM_ohlc, 4},
    {"_FM_fm_nan_ratio", (DL_FUNC) &_FM_fm_nan_ratio, 1},
    {"_FM_fm_inf_ratio", (DL_FUNC) &_FM_fm_inf_ratio, 1},
    {"_FM_fm_zero_ratio", (DL_FUNC) &_FM_fm_zero_ratio, 2},
    {"_FM_fm_skew", (DL_FUNC) &_FM_fm_skew, 1},
    {"_FM_fm_kurt", (DL_FUNC) &_FM_fm_kurt, 1},
    {"_FM_fcap", (DL_FUNC) &_FM_fcap, 3},
    {"_FM_log_trim", (DL_FUNC) &_FM_log_trim, 1},
    {"_FM_cb_rt", (DL_FUNC) &_FM_cb_rt, 1},
    {"_FM_sign_pow", (DL_FUNC) &_FM_sign_pow, 2},
    {"_FM_fm_all_equal", (DL_FUNC) &_FM_fm_all_equal, 3},
    {"_FM_vec_scale", (DL_FUNC) &_FM_vec_scale, 3},
    {"_FM_vec_scale_xy", (DL_FUNC) &_FM_vec_scale_xy, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_FM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
