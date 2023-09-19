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

template <typename T>
static std::pair<bool, double> _corr_vec_vec(const T* px, int len_x, SEXP data_y, int x_sign, int y_sign, bool is_rcor) {
    if (TYPEOF(data_y) == REALSXP) {
        int len_y = LENGTH(data_y);
        const double* py = REAL(data_y);
        double val_ = NAN;
        if (is_rcor) {
            val_ = ornate::rcor(px, py, std::min(len_x, len_y), y_sign, x_sign);
        } else {
            val_ = ornate::corr(px, py, std::min(len_x, len_y), x_sign, y_sign);
        }
        return {true, val_};
    } else if (TYPEOF(data_y) == INTSXP) {
        int len_y = LENGTH(data_y);
        const int* py = INTEGER(data_y);
        double val_ = NAN;
        if (is_rcor) {
            val_ = ornate::rcor(px, py, std::min(len_x, len_y), y_sign, x_sign);
        } else {
            val_ = ornate::corr(px, py, std::min(len_x, len_y), x_sign, y_sign);
        }
        return {true, val_};
    }
    return {false, NAN};
}

static std::pair<bool, double> _corr_vec_vec_ss(SEXP data_x, SEXP data_y, int x_sign, int y_sign, bool is_rcor) {
    if (TYPEOF(data_x) == REALSXP) {
        return _corr_vec_vec(REAL(data_x), LENGTH(data_x), data_y, x_sign, y_sign, is_rcor);
    } else if (TYPEOF(data_y) == INTSXP) {
        return _corr_vec_vec(INTEGER(data_x), LENGTH(data_x), data_y, x_sign, y_sign, is_rcor);
    }
    return {false, NAN};
}

template <typename T>
static void _corr_vec_list(const T* px, int len_x, int j_from, const List& y, const std::string& str_x,
                           int x_sign, int y_sign, bool is_rcor,
                           std::vector<std::string>& x_ret,
                           std::vector<std::string>& y_ret,
                           std::vector<double>& val_ret) {
    CharacterVector ycols = y.names();
    int col_len = ycols.size();
    for (int j = j_from; j < col_len; ++j) {  // y
        String str_y(ycols[j]);
        SEXP data_y = y[str_y];
        bool has_ret;
        double val_ = NAN;
        std::tie(has_ret, val_) = _corr_vec_vec(px, len_x, data_y, x_sign, y_sign, is_rcor);
        if (has_ret) {
            val_ret.push_back(val_);
            x_ret.push_back(str_x);
            y_ret.push_back(str_y);
        }
    }
}

static List _corr_list(const List& x, int x_sign, int y_sign, bool is_rcor) {
    CharacterVector xcols = x.names();
    int col_len = xcols.size();
    std::vector<std::string> x_ret;
    std::vector<std::string> y_ret;
    std::vector<double> val_ret;
    for (int i = 0; i < col_len; ++i) {  // x
        String str_x(xcols[i]);
        SEXP data_x = x[str_x];
        if (TYPEOF(data_x) == REALSXP) {
            int len_x = LENGTH(data_x);
            const double* px = REAL(data_x);
            _corr_vec_list(px, len_x, i + 1, x, str_x, x_sign, y_sign, is_rcor, x_ret, y_ret, val_ret);
        } else if (TYPEOF(data_x) == INTSXP) {
            int len_x = LENGTH(data_x);
            const int* px = INTEGER(data_x);
            _corr_vec_list(px, len_x, i + 1, x, str_x, x_sign, y_sign, is_rcor, x_ret, y_ret, val_ret);
        }
    }
    return List::create(_("x") = x_ret, _("y") = y_ret, _("cor") = val_ret);
}

static List _corr_list_list(const List& x, const List& y, int x_sign, int y_sign, bool is_rcor) {
    CharacterVector xcols = x.names();
    int col_len = xcols.size();
    CharacterVector ycols = y.names();
    int col_y_len = ycols.size();
    std::vector<std::string> x_ret;
    std::vector<std::string> y_ret;
    std::vector<double> val_ret;
    std::unordered_map<std::string, bool> calced;
    for (int i = 0; i < col_len; ++i) {  // x
        std::string str_x(xcols[i]);
        SEXP data_x = x[str_x];
        if (TYPEOF(data_x) == REALSXP) {
            int len_x = LENGTH(data_x);
            const double* px = REAL(data_x);
            for (int j = 0; j < col_y_len; ++j) {  // y
                std::string str_y(ycols[j]);
                std::string calc_name1 = str_x + '_' + str_y;
                std::string calc_name2 = str_y + '_' + str_x;
                if (calced[calc_name1] || calced[calc_name2] || calc_name1 == calc_name2) continue;

                SEXP data_y = y[str_y];
                bool has_ret;
                double val_ = NAN;
                std::tie(has_ret, val_) = _corr_vec_vec(px, len_x, data_y, x_sign, y_sign, is_rcor);
                if (has_ret) {
                    val_ret.push_back(val_);
                    x_ret.push_back(str_x);
                    y_ret.push_back(str_y);
                    calced[calc_name1] = true;
                }
            }
        } else if (TYPEOF(data_x) == INTSXP) {
            int len_x = LENGTH(data_x);
            const int* px = INTEGER(data_x);
            for (int j = 0; j < col_len; ++j) {  // y
                std::string str_y(ycols[j]);
                std::string calc_name1 = str_x + '_' + str_y;
                std::string calc_name2 = str_y + '_' + str_x;
                if (calced[calc_name1] || calced[calc_name2] || calc_name1 == calc_name2) continue;

                SEXP data_y = y[str_y];
                bool has_ret;
                double val_ = NAN;
                std::tie(has_ret, val_) = _corr_vec_vec(px, len_x, data_y, x_sign, y_sign, is_rcor);
                if (has_ret) {
                    val_ret.push_back(val_);
                    x_ret.push_back(str_x);
                    y_ret.push_back(str_y);
                    calced[calc_name1] = true;
                }
            }
        }
    }
    return List::create(_("x") = x_ret, _("y") = y_ret, _("cor") = val_ret);
}

List _corr_list_vec(SEXP x, SEXP y, int x_sign, int y_sign, bool is_rcor) {
    List xx = as<List>(x);
    CharacterVector xcols = xx.names();
    int col_len = xcols.size();

    if (TYPEOF(y) == REALSXP) {
        const double* py = REAL(y);
        int len_y = LENGTH(y);
        List ret(col_len);
        for (int i = 0; i < col_len; ++i) {
            String str_(xcols[i]);
            SEXP data = xx[str_];

            bool has_ret;
            double val_ = NAN;
            std::tie(has_ret, val_) = _corr_vec_vec(py, len_y, data, x_sign, y_sign, is_rcor);
            if (has_ret) {
                ret[i] = val_;
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
            SEXP data = xx[str_];
            bool has_ret;
            double val_ = NAN;
            std::tie(has_ret, val_) = _corr_vec_vec(py, len_y, data, x_sign, y_sign, is_rcor);
            if (has_ret) {
                ret[i] = val_;
            }
        }
        ret.attr("names") = xcols;
        return ret;
    }
    return List::create();
}

SEXP _corr_ss(SEXP x, SEXP y, int x_sign, int y_sign, bool is_rcor) {
    if (TYPEOF(y) == NILSXP) {
        return _corr_list(x, x_sign, y_sign, is_rcor);
    } else if (Rf_isVectorAtomic(x)) {
        if (Rf_isVectorAtomic(y)) {
            auto item = _corr_vec_vec_ss(x, y, x_sign, y_sign, is_rcor);
            return Rf_ScalarReal(item.second);
        } else if (Rf_isVectorList(y)) {
            return _corr_list_vec(y, x, x_sign, y_sign, is_rcor);
        }
    } else if (Rf_isVectorList(x)) {
        if (Rf_isVectorAtomic(y)) {
            return _corr_list_vec(x, y, x_sign, y_sign, is_rcor);
        } else if (Rf_isVectorList(y)) {
            return _corr_list_list(x, y, x_sign, y_sign, is_rcor);
        }
    }
    return List::create();
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
SEXP pcor(SEXP x, SEXP y = R_NilValue, int x_sign = 0, int y_sign = 0) {
    return _corr_ss(x, y, x_sign, y_sign, false);
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
SEXP rcor(SEXP x, SEXP y = R_NilValue, int x_sign = 0, int y_sign = 0) {
    return _corr_ss(x, y, x_sign, y_sign, true);
}