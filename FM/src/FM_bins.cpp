#include <Rcpp.h>
#include <math_stats_once.h>
#include <omp.h>

using namespace Rcpp;


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