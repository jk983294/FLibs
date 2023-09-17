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

//' pcor
//'
//' @param x List
//' @param y numeric vector
//' @param x_sign filter x
//' @param y_sign filter y
//' @return list
//' @export
// [[Rcpp::export]]
List pcor(const List& x, SEXP y = R_NilValue, int x_sign = 0, int y_sign = 0) {
    CharacterVector xcols = x.names();
    int col_len = xcols.size();

    if (TYPEOF(y) == REALSXP) {
        const double* py = REAL(y);
        int len_y = LENGTH(y);
        List ret(col_len);
        for (int i = 0; i < col_len; ++i) {
            String str_(xcols[i]);
            SEXP data = x[str_];
            if (TYPEOF(data) == REALSXP) {
                int len = LENGTH(data);
                const double* px = REAL(data);
                ret[i] = ornate::corr(px, py, std::min(len, len_y), x_sign, y_sign);
            } else if (TYPEOF(data) == INTSXP) {
                int len = LENGTH(data);
                const int* px = INTEGER(data);
                ret[i] = ornate::corr(px, py, std::min(len, len_y), x_sign, y_sign);
            }
        }
        ret.attr("names") = xcols;
        return ret;
    } else if (TYPEOF(y) == INTSXP) {
        const int* py = INTEGER(y);
        int len_y = LENGTH(y);
        List ret(col_len);
        for (int i = 0; i < col_len; ++i) {
            String str_(xcols[i]);
            SEXP data = x[str_];
            if (TYPEOF(data) == REALSXP) {
                int len = LENGTH(data);
                const double* px = REAL(data);
                ret[i] = ornate::corr(px, py, std::min(len, len_y), x_sign, y_sign);
            } else if (TYPEOF(data) == INTSXP) {
                int len = LENGTH(data);
                const int* px = INTEGER(data);
                ret[i] = ornate::corr(px, py, std::min(len, len_y), x_sign, y_sign);
            }
        }
        ret.attr("names") = xcols;
        return ret;
    } else if (TYPEOF(y) == NILSXP) {
        std::vector<std::string> x_ret;
        std::vector<std::string> y_ret;
        std::vector<double> val_ret;
        for (int i = 0; i < col_len; ++i) {  // x
            String str_x(xcols[i]);
            SEXP data_x = x[str_x];
            if (TYPEOF(data_x) == REALSXP) {
                int len_x = LENGTH(data_x);
                const double* px = REAL(data_x);
                for (int j = i + 1; j < col_len; ++j) {  // y
                    String str_y(xcols[j]);
                    SEXP data_y = x[str_y];
                    if (TYPEOF(data_y) == REALSXP) {
                        int len_y = LENGTH(data_y);
                        const double* py = REAL(data_y);
                        val_ret.push_back(ornate::corr(px, py, std::min(len_x, len_y), x_sign, y_sign));
                        x_ret.push_back(str_x);
                        y_ret.push_back(str_y);
                    } else if (TYPEOF(data_y) == INTSXP) {
                        int len_y = LENGTH(data_y);
                        const int* py = INTEGER(data_y);
                        val_ret.push_back(ornate::corr(px, py, std::min(len_x, len_y), x_sign, y_sign));
                        x_ret.push_back(str_x);
                        y_ret.push_back(str_y);
                    }
                }
            } else if (TYPEOF(data_x) == INTSXP) {
                int len_x = LENGTH(data_x);
                const int* px = INTEGER(data_x);
                for (int j = i + 1; j < col_len; ++j) {  // y
                    String str_y(xcols[j]);
                    SEXP data_y = x[str_y];
                    if (TYPEOF(data_y) == REALSXP) {
                        int len_y = LENGTH(data_y);
                        const double* py = REAL(data_y);
                        val_ret.push_back(ornate::corr(px, py, std::min(len_x, len_y), x_sign, y_sign));
                        x_ret.push_back(str_x);
                        y_ret.push_back(str_y);
                    } else if (TYPEOF(data_y) == INTSXP) {
                        int len_y = LENGTH(data_y);
                        const int* py = INTEGER(data_y);
                        val_ret.push_back(ornate::corr(px, py, std::min(len_x, len_y), x_sign, y_sign));
                        x_ret.push_back(str_x);
                        y_ret.push_back(str_y);
                    }
                }
            }
        }
        return List::create(_("x") = x_ret, _("y") = y_ret, _("cor") = val_ret);
    } else {
        return List::create();
    }
}

//' rcor
//'
//' @param x List
//' @param y numeric vector
//' @param x_sign filter x
//' @param y_sign filter y
//' @return list
//' @export
// [[Rcpp::export]]
List rcor(const List& x, SEXP y = R_NilValue, int x_sign = 0, int y_sign = 0) {
    CharacterVector xcols = x.names();
    int col_len = xcols.size();

    if (TYPEOF(y) == REALSXP) {
        const double* py = REAL(y);
        int len_y = LENGTH(y);
        List ret(col_len);
        for (int i = 0; i < col_len; ++i) {
            String str_(xcols[i]);
            SEXP data = x[str_];
            if (TYPEOF(data) == REALSXP) {
                int len = LENGTH(data);
                const double* px = REAL(data);
                ret[i] = ornate::rcor(px, py, std::min(len, len_y), y_sign, x_sign);
            } else if (TYPEOF(data) == INTSXP) {
                int len = LENGTH(data);
                const int* px = INTEGER(data);
                ret[i] = ornate::rcor(px, py, std::min(len, len_y), y_sign, x_sign);
            }
        }
        ret.attr("names") = xcols;
        return ret;
    } else if (TYPEOF(y) == INTSXP) {
        const int* py = INTEGER(y);
        int len_y = LENGTH(y);
        List ret(col_len);
        for (int i = 0; i < col_len; ++i) {
            String str_(xcols[i]);
            SEXP data = x[str_];
            if (TYPEOF(data) == REALSXP) {
                int len = LENGTH(data);
                const double* px = REAL(data);
                ret[i] = ornate::rcor(px, py, std::min(len, len_y), y_sign, x_sign);
            } else if (TYPEOF(data) == INTSXP) {
                int len = LENGTH(data);
                const int* px = INTEGER(data);
                ret[i] = ornate::rcor(px, py, std::min(len, len_y), y_sign, x_sign);
            }
        }
        ret.attr("names") = xcols;
        return ret;
    } else if (TYPEOF(y) == NILSXP) {
        std::vector<std::string> x_ret;
        std::vector<std::string> y_ret;
        std::vector<double> val_ret;
        for (int i = 0; i < col_len; ++i) {  // x
            String str_x(xcols[i]);
            SEXP data_x = x[str_x];
            if (TYPEOF(data_x) == REALSXP) {
                int len_x = LENGTH(data_x);
                const double* px = REAL(data_x);
                for (int j = i + 1; j < col_len; ++j) {  // y
                    String str_y(xcols[j]);
                    SEXP data_y = x[str_y];
                    if (TYPEOF(data_y) == REALSXP) {
                        int len_y = LENGTH(data_y);
                        const double* py = REAL(data_y);
                        val_ret.push_back(ornate::rcor(px, py, std::min(len_x, len_y), y_sign, x_sign));
                        x_ret.push_back(str_x);
                        y_ret.push_back(str_y);
                    } else if (TYPEOF(data_y) == INTSXP) {
                        int len_y = LENGTH(data_y);
                        const int* py = INTEGER(data_y);
                        val_ret.push_back(ornate::rcor(px, py, std::min(len_x, len_y), y_sign, x_sign));
                        x_ret.push_back(str_x);
                        y_ret.push_back(str_y);
                    }
                }
            } else if (TYPEOF(data_x) == INTSXP) {
                int len_x = LENGTH(data_x);
                const int* px = INTEGER(data_x);
                for (int j = i + 1; j < col_len; ++j) {  // y
                    String str_y(xcols[j]);
                    SEXP data_y = x[str_y];
                    if (TYPEOF(data_y) == REALSXP) {
                        int len_y = LENGTH(data_y);
                        const double* py = REAL(data_y);
                        val_ret.push_back(ornate::rcor(px, py, std::min(len_x, len_y), y_sign, x_sign));
                        x_ret.push_back(str_x);
                        y_ret.push_back(str_y);
                    } else if (TYPEOF(data_y) == INTSXP) {
                        int len_y = LENGTH(data_y);
                        const int* py = INTEGER(data_y);
                        val_ret.push_back(ornate::rcor(px, py, std::min(len_x, len_y), y_sign, x_sign));
                        x_ret.push_back(str_x);
                        y_ret.push_back(str_y);
                    }
                }
            }
        }
        return List::create(_("x") = x_ret, _("y") = y_ret, _("cor") = val_ret);
    } else {
        return List::create();
    }
}