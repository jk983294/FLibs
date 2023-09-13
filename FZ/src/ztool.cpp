#include <Rcpp.h>
#include <zerg_string.h>

using namespace Rcpp;

//' split string
//'
//' @param str string to split
//' @param separator string used to separate
//' @return vector of string
//' @export
// [[Rcpp::export]]
std::vector<std::string> fz_split(const std::string& str, const std::string& separator) {
    return ztool::splits(str, separator.c_str());
}