#include <Rcpp.h>
#include <math_stats.h>

using namespace Rcpp;

//' fm_nan_ratio
//'
//' @param x vector
//' @export
// [[Rcpp::export]]
double fm_nan_ratio(const std::vector<double>& x) {
    int cnt = 0;
    int num = x.size();
    for (int i = 0; i < num; ++i) {
        if (std::isnan(x[i])) cnt++;
    }
    if (num > 0)
        return (double)cnt / num;
    else
        return NAN;
}

//' fm_inf_ratio
//'
//' @param x vector
//' @return inf_ratio exclude nan
//' @export
// [[Rcpp::export]]
double fm_inf_ratio(const std::vector<double>& x) {
    int cnt = 0;
    int num = x.size();
    for (int i = 0; i < num; ++i) {
        if (std::isinf(x[i])) cnt++;
    }
    if (num > 0)
        return (double)cnt / num;
    else
        return NAN;
}

//' fm_zero_ratio
//'
//' @param x vector
//' @param precision precision
//' @return zero_ratio
//' @export
// [[Rcpp::export]]
double fm_zero_ratio(SEXP x, double precision = 1e-9) {
    if (TYPEOF(x) == REALSXP) {
        const double* px = REAL(x);
        int cnt = 0;
        int num = LENGTH(x);
        for (int i = 0; i < num; ++i) {
            double v = px[i];
            if (std::isfinite(v) && std::abs(v) < precision) cnt++;
        }
        if (num > 0)
            return (double)cnt / num;
        else
            return NAN;
    } else if (TYPEOF(x) == INTSXP) {
        const int* px = INTEGER(x);
        int cnt = 0;
        int num = LENGTH(x);
        for (int i = 0; i < num; ++i) {
            if (px[i] == 0) cnt++;
        }
        if (num > 0)
            return (double)cnt / num;
        else
            return NAN;
    } else {
        return NAN;
    }
}

//' fm_skew
//'
//' @param x vector
//' @return skew
//' @export
// [[Rcpp::export]]
double fm_skew(const std::vector<double>& x) { return ornate::math_skew(x); }

//' fm_kurt
//'
//' @param x vector
//' @return kurt
//' @export
// [[Rcpp::export]]
double fm_kurt(const std::vector<double>& x) { return ornate::math_kurtosis(x); }

//' auto pcor
//'
//' @param dt List
//' @param lags lags
//' @return list
//' @export
// [[Rcpp::export]]
List autopcor(const List& dt, const std::vector<int>& lags) {
    CharacterVector xcols = dt.names();
    int col_len = xcols.size();
    List ret(col_len);
    for (int i = 0; i < col_len; ++i) {
        String str_(xcols[i]);
        SEXP data = dt[str_];
        if (TYPEOF(data) == REALSXP) {
            int len = LENGTH(data);
            const double* pd = REAL(data);
            std::vector<double> _auto_pcor = ornate::auto_pcor(pd, lags, len);
            ret[i] = _auto_pcor;
        } else if (TYPEOF(data) == INTSXP) {
            int len = LENGTH(data);
            const int* pd = INTEGER(data);
            std::vector<double> _auto_pcor = ornate::auto_pcor(pd, lags, len);
            ret[i] = _auto_pcor;
        }
    }
    ret.attr("names") = xcols;
    return ret;
}