#include <Rcpp.h>
#include <omp.h>

using namespace Rcpp;


//' fast_quantile_kernel
//'
//' @param x double vector
//' @param group int vector
//' @param qs double vector contains quantiles
//' @param threads default = 1
//' @export
// [[Rcpp::export]]
List fast_quantile_kernel(const NumericVector& x, const IntegerVector& group, std::vector<double> qs, int threads = 1) {
    std::vector<int> g;
    std::vector<std::vector<double>> q;
    std::unordered_map<int, std::vector<long>*> m;
    long size = x.size();
    const double* px = REAL(x);
    long size1 = group.size();
    const int* pg = INTEGER(group);

    if (size1 != size) return List::create();

    for (long i = 0; i < size1; ++i) {
        std::vector<long>* pm = m[pg[i]];
        if (pm == nullptr) {
            pm = new std::vector<long>();
            pm->reserve(1000);
            m[pg[i]] = pm;
        }
        pm->push_back(i);
    }
    std::transform(m.begin(), m.end(), std::back_inserter(g), [](auto& item) {
        return item.first;
    });
    std::sort(g.begin(), g.end());
    q.resize(qs.size(), std::vector<double>(g.size(), NAN));

#pragma omp parallel for num_threads(threads == 0 ? omp_get_max_threads():threads)
    for (size_t i = 0; i < g.size(); i++) {
        std::vector<long>* pm = m[g[i]];
        std::vector<double> vec;
        vec.reserve(pm->size());
        for (long idx : *pm) {
            double val = px[idx];
            if (std::isfinite(val)) {
                vec.push_back(val);
            }
        }
        for (size_t j = 0; j < qs.size(); j++) {
            std::size_t ny = vec.size();
            double res = NAN;
            if (ny != 0) {
                double idx = (ny - 1) * qs[j];
                double idx_lb = std::floor(idx);
                double idx_ub = std::ceil(idx);
                if (idx_lb == idx_ub) {
                    std::nth_element(vec.begin(), vec.begin() + idx, vec.end());
                    res = vec[idx];
                } else {
                    std::nth_element(vec.begin(), vec.begin() + idx_ub, vec.end());
                    std::nth_element(vec.begin(), vec.begin() + idx_lb, vec.begin() + idx_ub);
                    res = vec[idx_lb] * (idx_ub - idx) + vec[idx_ub] * (idx - idx_lb);
                }
            }
            q[j][i] = res;
        }
    }
    List out(1 + qs.size());
    CharacterVector out_names(1 + qs.size());
    out[0] = g;
    out_names[0] = "group";
    for (size_t j = 0; j < qs.size(); j++) {
        out[j + 1] = q[j];
        out_names[j + 1] = "p" + std::to_string(qs[j] * 100);
    }
    out.attr("names") = out_names;

    for (auto& item : m) delete item.second;
    return out;
}

double __quantile(std::vector<double>& vec, double q) {
    std::size_t ny = vec.size();
    double res = NAN;
    if (ny != 0) {
        double idx = (ny - 1) * q;
        double idx_lb = std::floor(idx);
        double idx_ub = std::ceil(idx);
        if (idx_lb == idx_ub) {
            std::nth_element(vec.begin(), vec.begin() + idx, vec.end());
            res = vec[idx];
        } else {
            std::nth_element(vec.begin(), vec.begin() + idx_ub, vec.end());
            std::nth_element(vec.begin(), vec.begin() + idx_lb, vec.begin() + idx_ub);
            res = vec[idx_lb] * (idx_ub - idx) + vec[idx_ub] * (idx - idx_lb);
        }
    }
    return res;
}

//' dt_quantile
//'
//' @param x List
//' @param q quantile
//' @param threads = 1
//' @return list
//' @export
// [[Rcpp::export]]
List dt_quantile(const List& x, double q, int threads = 1) {
    CharacterVector xcols = x.names();
    int col_len = xcols.size();
    std::vector<double> val_ret(col_len, NAN);
    
#pragma omp parallel for num_threads(threads == 0 ? omp_get_max_threads():threads)
    for (int i = 0; i < col_len; ++i) {  // x
        std::string str_x(xcols[i]);
        SEXP data_x = x[str_x];
        if (TYPEOF(data_x) == REALSXP) {
            int len_x = LENGTH(data_x);
            const double* px = REAL(data_x);
            std::vector<double> vec(px, px + len_x);
            val_ret[i] = __quantile(vec, q);
        }
    }
    return List::create(_("name") = xcols, _("quantile") = val_ret);
}