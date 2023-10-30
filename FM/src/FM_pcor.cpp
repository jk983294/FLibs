#include <Rcpp.h>
#include <math_stats.h>
#include <omp.h>

using namespace Rcpp;


//' auto pcor
//'
//' @param dt List
//' @param lags lags
//' @return list
//' @export
// [[Rcpp::export]]
List autopcor(const List& dt, const std::vector<int>& lags, int threads = 1) {
    CharacterVector xcols = dt.names();
    int col_len = xcols.size();
    List ret(col_len);
#pragma omp parallel for num_threads(threads == 0 ? omp_get_max_threads():threads)
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

struct CorResult {
    std::string x_label;
    std::string y_label;
    bool has_ret{false};
    double val_ = NAN;
};

template <typename T>
static std::vector<CorResult> _corr_vec_list(const T* px, int len_x, int j_from, const List& y, const std::string& str_x, int x_sign,
                           int y_sign, bool is_rcor, int threads) {
    CharacterVector ycols = y.names();
    int col_len = ycols.size();
    std::vector<CorResult> rets(col_len - j_from);
#pragma omp parallel for num_threads(threads == 0 ? omp_get_max_threads():threads)
    for (int j = j_from; j < col_len; ++j) {  // y
        String str_y(ycols[j]);
        SEXP data_y = y[str_y];
        bool has_ret;
        double val_ = NAN;
        std::tie(has_ret, val_) = _corr_vec_vec(px, len_x, data_y, x_sign, y_sign, is_rcor);
        rets[j - j_from] = {str_x, str_y, has_ret, val_};
    }
    return rets;
}

static List _corr_list(const List& x, int x_sign, int y_sign, bool is_rcor, int threads) {
    CharacterVector xcols = x.names();
    int col_len = xcols.size();
    std::vector<std::vector<CorResult>> rets(col_len);
#pragma omp parallel for num_threads(threads == 0 ? omp_get_max_threads():threads)
    for (int i = 0; i < col_len; ++i) {  // x
        String str_x(xcols[i]);
        SEXP data_x = x[str_x];
        if (TYPEOF(data_x) == REALSXP) {
            int len_x = LENGTH(data_x);
            const double* px = REAL(data_x);
            rets[i] = _corr_vec_list(px, len_x, i + 1, x, str_x, x_sign, y_sign, is_rcor, threads);
        } else if (TYPEOF(data_x) == INTSXP) {
            int len_x = LENGTH(data_x);
            const int* px = INTEGER(data_x);
            rets[i] = _corr_vec_list(px, len_x, i + 1, x, str_x, x_sign, y_sign, is_rcor, threads);
        }
    }

    std::vector<std::string> x_ret;
    std::vector<std::string> y_ret;
    std::vector<double> val_ret;

    for (auto& rets1 : rets) {
        for (auto& ret : rets1) {
            if (ret.has_ret) {
                val_ret.push_back(ret.val_);
                x_ret.push_back(ret.x_label);
                y_ret.push_back(ret.y_label);
            }
        }
    }
    return List::create(_("x") = x_ret, _("y") = y_ret, _("cor") = val_ret);
}

static List _corr_list_list(const List& x, const List& y, int x_sign, int y_sign, bool is_rcor, int threads) {
    CharacterVector xcols = x.names();
    int col_len = xcols.size();
    CharacterVector ycols = y.names();
    int col_y_len = ycols.size();
    std::vector<std::vector<CorResult>> rets(col_len, std::vector<CorResult>(col_y_len));

    std::unordered_map<std::string, bool> calced;
    for (int i = 0; i < col_len; ++i) {  // x
        std::string str_x(xcols[i]);
        for (int j = 0; j < col_y_len; ++j) {  // y
            std::string str_y(ycols[j]);
            std::string calc_name1 = str_x + '_' + str_y;
            std::string calc_name2 = str_y + '_' + str_x;
            if (calced[calc_name1] || calced[calc_name2] || calc_name1 == calc_name2) continue;
            rets[i][j] = {str_x, str_y, true, NAN};
            calced[calc_name1] = true;
        }
    }

    for (int i = 0; i < col_len; ++i) {  // x
        std::string str_x(xcols[i]);
        SEXP data_x = x[str_x];
        if (TYPEOF(data_x) == REALSXP) {
            int len_x = LENGTH(data_x);
            const double* px = REAL(data_x);
#pragma omp parallel for num_threads(threads == 0 ? omp_get_max_threads():threads)
            for (int j = 0; j < col_y_len; ++j) {  // y
                if (not rets[i][j].has_ret) continue;
                std::string str_y(ycols[j]);
                SEXP data_y = y[str_y];
                bool has_ret;
                double val_ = NAN;
                std::tie(has_ret, val_) = _corr_vec_vec(px, len_x, data_y, x_sign, y_sign, is_rcor);
                rets[i][j] = {str_x, str_y, has_ret, val_};
            }
        } else if (TYPEOF(data_x) == INTSXP) {
            int len_x = LENGTH(data_x);
            const int* px = INTEGER(data_x);
#pragma omp parallel for num_threads(threads == 0 ? omp_get_max_threads():threads)
            for (int j = 0; j < col_len; ++j) {  // y
                if (not rets[i][j].has_ret) continue;
                std::string str_y(ycols[j]);
                SEXP data_y = y[str_y];
                bool has_ret;
                double val_ = NAN;
                std::tie(has_ret, val_) = _corr_vec_vec(px, len_x, data_y, x_sign, y_sign, is_rcor);
                rets[i][j] = {str_x, str_y, has_ret, val_};
            }
        }
    }

    std::vector<std::string> x_ret;
    std::vector<std::string> y_ret;
    std::vector<double> val_ret;
    for (auto& rets1 : rets) {
        for (auto& ret : rets1) {
            if (ret.has_ret) {
                val_ret.push_back(ret.val_);
                x_ret.push_back(ret.x_label);
                y_ret.push_back(ret.y_label);
            }
        }
    }
    return List::create(_("x") = x_ret, _("y") = y_ret, _("cor") = val_ret);
}

List _corr_list_vec(SEXP x, SEXP y, int x_sign, int y_sign, bool is_rcor, int threads) {
    List xx = as<List>(x);
    CharacterVector xcols = xx.names();
    int col_len = xcols.size();

    if (TYPEOF(y) == REALSXP) {
        const double* py = REAL(y);
        int len_y = LENGTH(y);
        List ret(col_len);
#pragma omp parallel for num_threads(threads == 0 ? omp_get_max_threads():threads)
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
#pragma omp parallel for num_threads(threads == 0 ? omp_get_max_threads():threads)     
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

SEXP _corr_ss(SEXP x, SEXP y, int x_sign, int y_sign, bool is_rcor, int threads) {
    if (TYPEOF(y) == NILSXP) {
        return _corr_list(x, x_sign, y_sign, is_rcor, threads);
    } else if (Rf_isVectorAtomic(x)) {
        if (Rf_isVectorAtomic(y)) {
            auto item = _corr_vec_vec_ss(x, y, x_sign, y_sign, is_rcor);
            return Rf_ScalarReal(item.second);
        } else if (Rf_isVectorList(y)) {
            return _corr_list_vec(y, x, x_sign, y_sign, is_rcor, threads);
        }
    } else if (Rf_isVectorList(x)) {
        if (Rf_isVectorAtomic(y)) {
            return _corr_list_vec(x, y, x_sign, y_sign, is_rcor, threads);
        } else if (Rf_isVectorList(y)) {
            return _corr_list_list(x, y, x_sign, y_sign, is_rcor, threads);
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
SEXP pcor(SEXP x, SEXP y = R_NilValue, int x_sign = 0, int y_sign = 0, int threads = 1) { 
    return _corr_ss(x, y, x_sign, y_sign, false, threads); 
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
SEXP rcor(SEXP x, SEXP y = R_NilValue, int x_sign = 0, int y_sign = 0, int threads = 1) { 
    return _corr_ss(x, y, x_sign, y_sign, true, threads);
}