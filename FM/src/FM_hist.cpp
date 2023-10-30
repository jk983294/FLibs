#include <Rcpp.h>
#include <math_stats_once.h>
#include <omp.h>

using namespace Rcpp;

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
