#include <Rcpp.h>
#include <math_stats.h>
#include <math_stats_once.h>
#include <omp.h>

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

static double fcap_internal(double x, double lower, double upper) {
    if (x > upper)
        return upper;
    else if (x < lower)
        return lower;
    else
        return x;
}

//' fcap
//'
//' @param x List
//' @param lower lower
//' @param upper upper
//' @export
// [[Rcpp::export]]
SEXP fcap(SEXP x, double lower = -1., double upper = 1.) {
    if (Rf_isVectorAtomic(x)) {
        int len = LENGTH(x);
        NumericVector ret(len, NAN);
        double* pRet = REAL(ret);
        if (TYPEOF(x) == REALSXP) {
            double* px = REAL(x);
            for (int i = 0; i < len; i++) {
                pRet[i] = fcap_internal(px[i], lower, upper);
            }
        } else if (TYPEOF(x) == INTSXP) {
            int* px = INTEGER(x);
            for (int i = 0; i < len; i++) {
                pRet[i] = fcap_internal(px[i], lower, upper);
            }
        }
        return ret;
    } else if (Rf_isNumeric(x)) {
        double val = Rf_asReal(x);
        return Rf_ScalarReal(fcap_internal(val, lower, upper));
    }
    return Rf_ScalarReal(NAN);
}

//' log_trim
//'
//' @param x vector
//' @export
// [[Rcpp::export]]
std::vector<double> log_trim(const std::vector<double>& x) {
    int n = x.size();
    std::vector<double> ret(n, NAN);
    for (int i = 0; i < n; ++i) {
        if (x[i] > 1)
            ret[i] = std::log(x[i]) + 1;
        else if (x[i] < -1)
            ret[i] = -(std::log(-x[i]) + 1);
        else
            ret[i] = x[i];
    }
    return ret;
}

//' cube root
//'
//' @param x vector
//' @export
// [[Rcpp::export]]
std::vector<double> cb_rt(const std::vector<double>& x) {
    int n = x.size();
    std::vector<double> ret(n, NAN);
    for (int i = 0; i < n; ++i) {
        ret[i] = std::cbrt(x[i]);
    }
    return ret;
}

static int sign(double x) {
    if (x > 1e-9)
        return 1;
    else if (x < -1e-9)
        return -1;
    else
        return 0;
}

//' sign_pow
//'
//' @param x vector
//' @export
// [[Rcpp::export]]
std::vector<double> sign_pow(const std::vector<double>& x, double exp) {
    int n = x.size();
    std::vector<double> ret(n, NAN);
    for (int i = 0; i < n; ++i) {
        ret[i] = sign(x[i]) * std::pow(std::abs(x[i]), exp);
    }
    return ret;
}

//' all_equal
//'
//' @param x vector
//' @export
// [[Rcpp::export]]
bool fm_all_equal(const std::vector<double>& x, double tol = 1e-8, bool na_rm = false) {
    size_t n = x.size();
    if (n <= 0) return false;
    double first = x.front();
    if (std::isnan(first)) {
        for (size_t i = 1; i < n; ++i) {
            if (!std::isnan(x[i])) return false;
        }
    } else {
        for (size_t i = 1; i < n; ++i) {
            if (!std::isnan(x[i])) {
                if (std::abs(x[i] - first) > tol) return false;
            } else if (not na_rm)
                return false;
        }
    }
    return true;
}


//' scale vector based on given mean/sd
//'
//' @param x vector
//' @param mean target mean
//' @param sd target sd
//' @export
// [[Rcpp::export]]
NumericVector vec_scale(const NumericVector& x, double mean, double sd) {
    long size = x.size();
    const double* px = REAL(x);
    NumericVector rets(size, NAN);
    double* pr = REAL(rets);
    if (std::isfinite(sd) && sd > 1e-9) {
        ornate::rolling_sm_once rso;
        for (long i = 0; i < size; i++) {
            rso(px[i]);
        }
        double mean1, sd1;
        std::tie(mean1, sd1) = rso.final();
        if (std::isfinite(sd1) && sd1 > 1e-9) {
            double factor = sd / sd1;
            for (long i = 0; i < size; i++) {
                pr[i] = (px[i] - mean1) * factor + mean;
            }
        }
    }
    return rets;
}

//' scale vector based on given y's mean/sd
//'
//' @param x vector
//' @param mean target mean
//' @param sd target sd
//' @export
// [[Rcpp::export]]
NumericVector vec_scale_xy(const NumericVector& x, const NumericVector& y) {
    long size = y.size();
    const double* py = REAL(y);
    ornate::rolling_sm_once rso;
    for (long i = 0; i < size; i++) {
        rso(py[i]);
    }
    double mean, sd;
    std::tie(mean, sd) = rso.final();
    return vec_scale(x, mean, sd);
}