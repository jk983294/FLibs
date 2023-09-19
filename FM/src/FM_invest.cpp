#include <Rcpp.h>
#include <math_invest.h>

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