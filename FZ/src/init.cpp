#include <Rcpp.h>
#include <zerg/time/time.h>

using namespace Rcpp;

// [[Rcpp::export]]
void initPackage() {
    zerg::check_to(0);
}