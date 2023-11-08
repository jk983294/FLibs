#include <Rcpp.h>

using namespace Rcpp;

//' pvalue_feature_select
//'
//' @param pvalues pvalues
//' @param threshold threshold
//' @param has_intercept has_intercept
//' @return selected feature idx
//' @export
// [[Rcpp::export]]
std::vector<bool> pvalue_feature_select(const std::vector<double>& pvalues, double threshold, bool has_intercept) {
    std::vector<bool> selected;
    int n = pvalues.size();
    int offset = 0;
    if (has_intercept) offset = 1;
    if (n > 1) {
        selected.resize(n - offset, false);
        for (int i = offset; i < n; i++) {
            if (pvalues[i] < threshold) selected[i - offset] = true;
        }
    }
    return selected;
}