#include <Rcpp.h>
#include <math_random_walk.h>
#include <math_stats.h>

using namespace Rcpp;

//' general brownian motion
//'
//' @param ukey ukey
//' @param start_price start_price
//' @param n length
//' @param DataDate date
//' @return list
//' @export
// [[Rcpp::export]]
List GBM_ohlc(int ukey = 1, double start_price = 100, int n = 1000, int DataDate = 1) {
    std::vector<double> close, open, high, low, volume;
    ornate::GBM(close, start_price, n, 0.03, 0., 0.02);
    ornate::GBM_ohlc(close, open, high, low);
    volume = ornate::generate_gaussian<double>(n, 10000., 1000.);
    std::vector<int> ukey_(n, ukey);
    std::vector<int> date(n, DataDate);
    std::vector<int> tick(n, 0);
    std::iota(tick.begin(), tick.end(), 1);
    return List::create(_("ukey") = ukey_, _("ticktime") = tick, _("DataDate") = date, _("open") = open,
                        _("high") = high, _("low") = low, _("close") = close, _("volume") = volume);
}