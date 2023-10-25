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
static std::pair<bool, double> _corr_vec_vec(const T* px, int len_x, SEXP data_y, int x_sign, int y_sign,
                                             bool is_rcor) {
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
static void _corr_vec_list(const T* px, int len_x, int j_from, const List& y, const std::string& str_x, int x_sign,
                           int y_sign, bool is_rcor, std::vector<std::string>& x_ret, std::vector<std::string>& y_ret,
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
SEXP pcor(SEXP x, SEXP y = R_NilValue, int x_sign = 0, int y_sign = 0) { return _corr_ss(x, y, x_sign, y_sign, false); }

//' rcor
//'
//' @param x List
//' @param y numeric vector
//' @param x_sign filter x
//' @param y_sign filter y
//' @return list
//' @export
// [[Rcpp::export]]
SEXP rcor(SEXP x, SEXP y = R_NilValue, int x_sign = 0, int y_sign = 0) { return _corr_ss(x, y, x_sign, y_sign, true); }

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

vector<vector<size_t>> get_bins(const double* x, size_t total, size_t n, size_t& per_n, const vector<bool>& mask) {
    std::vector<std::pair<double, size_t>> datum;
    datum.reserve(total);
    for (size_t i = 0; i < total; ++i) {
        if ((!mask.empty() && mask[i]) || std::isfinite(x[i])) {
            datum.emplace_back(x[i], i);
        }
    }
    std::sort(datum.begin(), datum.end(), [](const auto& l, const auto& r) {
        return l.first < r.first;
    });
    vector<vector<size_t>> rets(n);
    per_n = std::lround(std::ceil((double)datum.size() / n));
    for (size_t i = 0; i < n; i++) {
        size_t left = per_n * i;
        size_t right = left + per_n;
        if (left >= datum.size()) break;
        if (right > datum.size()) right = datum.size();
        for (size_t j = left; j < right; j++) {
            rets[i].push_back(datum[j].second);
        }
    }
    return rets;
}

vector<vector<double>> get_binned_stats(const double* x, const double* y, size_t total, size_t n, size_t& per_n, int threads) {
    vector<bool> mask(total, false);
    for (size_t i = 0; i < total; ++i) {
        if (std::isfinite(x[i]) && std::isfinite(y[i])) {
            mask[i] = true;
        }
    }

    vector<vector<double>> rets(4, vector<double>(n, NAN));
    vector<vector<size_t>> bins = get_bins(x, total, n, per_n, mask);
#pragma omp parallel for num_threads(threads == 0 ? omp_get_max_threads():threads)
    for (size_t i = 0; i < n; i++) {
        ornate::rolling_sm_once rso_x, rso_y;
        for (size_t idx : bins[i]) {
            rso_x(x[idx]);
            rso_y(y[idx]);
        }
        double mean, sd;
        std::tie(mean, sd) = rso_x.final();
        rets[2][i] = mean;
        rets[0][i] = sd;
        std::tie(mean, sd) = rso_y.final();
        rets[3][i] = mean;
        rets[1][i] = sd;
    }
    return rets;
}

//' get_binned_stats
//'
//' @param x vector, bin divided by x, not y
//' @param y vector
//' @param n_bin bin num
//' @param threads thread num
//' @export
// [[Rcpp::export]]
List get_binned_stats(const NumericVector& x, const NumericVector& y, size_t n_bin, int threads = 1) {
    size_t per_n;
    vector<vector<double>> tmp = get_binned_stats(REAL(x), REAL(y), x.size(), n_bin, per_n, threads);
    return List::create(_("x_sd") = tmp[0], _("y_sd") = tmp[1], _("x_mean") = tmp[2], _("y_mean") = tmp[3], _("per_n") = per_n);
}


void fast_hist(const double* x, size_t n, size_t bins, double min_, double max_,
               vector<double>& vals, vector<double>& cnts,
               double& mean_, double& sd_, double& skew_, double& kurt_, int threads) {
    if (bins == 0) return;
    vals.resize(bins + 1, NAN);
    cnts.resize(bins, 0);
    if(threads == 0) threads = omp_get_max_threads();
    size_t per_n = std::lround(std::ceil((double)n / threads));
    if (per_n <= 2) {
        threads = 1;
        per_n = n;
    }

    ornate::rolling_all_once ra;
    vector<ornate::rolling_all_once> ras(threads);
#pragma omp parallel for num_threads(threads)
    for (int ti = 0; ti < threads; ++ti) {
        size_t left = per_n * ti;
        size_t right = left + per_n;
        if (left < n) {
            if (right > n) right = n;
            ornate::rolling_all_once& local_ra = ras[ti];
            for (size_t j = left; j < right; j++) {
                double val = x[j];
                if (std::isfinite(val)) {
                    if (!std::isnan(min_) && val < min_) continue;
                    if (!std::isnan(max_) && val > max_) continue;
                    local_ra(val);
                }
            }
        }
    }

    for (int ti = 0; ti < threads; ++ti) {
        ra.merge(ras[ti]);
    }
    if (ra.cnt <= 0) return;
    double nvalids = ra.cnt;
    if (std::isnan(min_)) min_ = ra.low;
    if (std::isnan(max_)) max_ = ra.high;
    if (std::isnan(min_) || std::isnan(max_)) return;
    std::tie(mean_, sd_) = ra.get_mean_sd();
    skew_ = ra.get_skew();
    kurt_ = ra.get_kurt();

    if (max_ - min_ < 1e-9) {
        vals = {min_, max_};
        cnts = {nvalids};
        return;
    }
    double normx = bins / (max_ - min_);
    for (size_t j = 0; j < bins + 1; ++j) {
        vals[j] = min_ + j * (max_ - min_) / bins;
    }

    int n_bin = static_cast<int>(bins);

    vector<vector<double>> tmp_cnts(threads, vector<double>(bins, 0));
#pragma omp parallel for num_threads(threads)
    for (int ti = 0; ti < threads; ++ti) {
        size_t left = per_n * ti;
        size_t right = left + per_n;
        if (left < n) {
            if (right > n) right = n;
            vector<double>& tmp_cnt = tmp_cnts[ti];
            for (size_t j = left; j < right; j++) {
                double val = x[j];
                if (std::isfinite(val) && val >= min_ && val <= max_) {
                    int ix = static_cast<int>((val - min_) * normx);
                    if (ix < 0) ix = 0;
                    if (ix >= n_bin) ix = n_bin - 1;
                    tmp_cnt[ix] += 1;
                }
            }
        }
    }

    for (int ti = 0; ti < threads; ++ti) {
        for (int bi = 0; bi < n_bin; ++bi) {
            cnts[bi] += tmp_cnts[ti][bi];
        }
    }
}

//' fast_hist_n
//'
//' @param x vector
//' @param bins bin num
//' @param min_val min val
//' @param max_val max val
//' @param threads thread num
//' @export
// [[Rcpp::export]]
List fast_hist_n(const NumericVector& x, size_t bins, double min_val = NA_REAL, double max_val = NA_REAL, int threads = 1) {
    vector<double> vals, cnts;
    double mean_, sd_, skew_, kurt_;
    fast_hist(REAL(x), x.size(), bins, min_val, max_val, vals, cnts, mean_, sd_, skew_, kurt_, threads);
    return List::create(_("cnts") = cnts, _("vals") = vals, _("mean_") = mean_, _("sd_") = sd_, _("skew_") = skew_, _("kurt_") = kurt_);
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