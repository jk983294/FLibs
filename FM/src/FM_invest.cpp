#include <Rcpp.h>
#include <math_invest.h>
#include <math_stats.h>

using namespace Rcpp;

//' fm_get_ret
//'
//' @param x vector
//' @param lag lag
//' @param skip skip
//' @export
// [[Rcpp::export]]
std::vector<double> fm_get_ret(const std::vector<double>& x, int lag, int skip = 0) {
    return ornate::calc_ret(x, lag, skip);
}

//' t0_nav
//'
//' @param signals vector
//' @param ret1 next bar's return
//' @param long_t >= threshold, open long
//' @param short_t <= threshold, open short
//' @param cost cost
//' @export
// [[Rcpp::export]]
std::vector<double> t0_nav(const std::vector<double>& signals, const std::vector<double>& ret1,
    double long_t, double short_t, double cost = 0) {
    return ornate::t0_nav(signals.data(), ret1.data(), signals.size(), long_t, short_t, cost);
}

//' factor_nav, nav = cumsum(factor * ret1)
//'
//' @param signals vector
//' @param ret1 next bar's return
//' @export
// [[Rcpp::export]]
std::vector<double> factor_nav(const std::vector<double>& signals, const std::vector<double>& ret1) {
    std::vector<double> res(signals.size(), 0);
    double nav = 0;
    for (size_t i = 0; i < signals.size(); i++) {
        if (std::isfinite(signals[i]) && std::isfinite(ret1[i])) {
            nav += signals[i] * ret1[i];
        }
        res[i] = nav;
    }
    return res;
}

//' sharpe, mean / sd
//'
//' @param rets vector
//' @export
// [[Rcpp::export]]
double sharpe(const std::vector<double>& rets) {
    return ornate::ts_sharpe(rets);
}