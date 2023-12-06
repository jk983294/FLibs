
#include <Rcpp.h>
#include <unordered_map>
#include <string>
using namespace Rcpp;

//' select_xx_xy
//'
//' @param xx_dt generate by ft[, FM::pcor(.SD), .SDcols= cols]
//' @param pcor_sorted_x, x vec sorted by pcor, could has dupe
//' @param xx_threshold, pcor threshold
//' @return list
//' @export
// [[Rcpp::export]]
std::vector<std::string> select_xx_xy(const List& xx_dt, const std::vector<std::string>& pcor_sorted_x, double xx_threshold = 0.8) {
    std::vector<std::string> pcor_unique_x; // rm dupe
    std::unordered_map<std::string, int> name2xy_idx;
    for (size_t i = 0; i < pcor_sorted_x.size(); i++) {
        if (name2xy_idx.count(pcor_sorted_x[i]) == 0) {
            name2xy_idx[pcor_sorted_x[i]] = pcor_unique_x.size();
            pcor_unique_x.push_back(pcor_sorted_x[i]);
        }
    }
    
    SEXP x_data = xx_dt["x"];
    SEXP y_data = xx_dt["y"];
    SEXP cor_data = xx_dt["cor"];
    const double* pcor_data = nullptr;
    // CharacterVector xv, yv;
    std::vector<std::string> xv, yv;
    int length = 0;
    
    if (TYPEOF(cor_data) == REALSXP) {
        pcor_data = REAL(cor_data);
        length = LENGTH(cor_data);
    }
    if (TYPEOF(x_data) == STRSXP) {
        xv = as<std::vector<std::string>>(x_data);
    }
    if (TYPEOF(y_data) == STRSXP) {
        yv = as<std::vector<std::string>>(y_data);
    }

    if (xv.size() == 0 || yv.size() == 0 || pcor_data == nullptr) {
        printf("xx_dt missing x/y/cor\n");
        return {};
    }

    std::vector<std::string> xx_unique_x; // rm dupe
    std::unordered_map<std::string, int> name2xx_idx;
    for (int i = 0; i < length; i++) {
        if (name2xx_idx.count(xv[i]) == 0) {
            name2xx_idx[xv[i]] = xx_unique_x.size();
            xx_unique_x.push_back(xv[i]);
        }
        if (name2xx_idx.count(yv[i]) == 0) {
            name2xx_idx[yv[i]] = xx_unique_x.size();
            xx_unique_x.push_back(yv[i]);
        }
    }
    int xx_len = xx_unique_x.size();
    std::vector<double> cor_mat(xx_len * xx_len, NAN);
    for (int i = 0; i < length; i++) {
        int x_idx = name2xx_idx[xv[i]];
        int y_idx = name2xx_idx[yv[i]];
        cor_mat[x_idx * xx_len + y_idx] = std::abs(pcor_data[i]);
        cor_mat[y_idx * xx_len + x_idx] = std::abs(pcor_data[i]);
    }
    
    std::vector<bool> selected(pcor_unique_x.size(), true);
    for (size_t i = 0; i < pcor_unique_x.size(); i++) {
        if (not selected[i]) continue;
        if (name2xx_idx.count(pcor_unique_x[i]) == 0) {
            selected[i] = false; // not in xx cor_mat, discard
            continue;
        }
        int x_idx = name2xx_idx[pcor_unique_x[i]];
        for (int y_idx = 0; y_idx < xx_len; y_idx++) {
            if (x_idx == y_idx) continue;
            if (name2xy_idx.count(xx_unique_x[y_idx]) == 0) continue;
            int pcor_xy_idx = name2xy_idx[xx_unique_x[y_idx]];
            double cor_ = cor_mat[x_idx * xx_len + y_idx];
            if (std::isnan(cor_) || cor_ > xx_threshold) {
                selected[pcor_xy_idx] = false;
                //printf("discard x=%s, y=%s cor=%f\n", pcor_unique_x[i].c_str(), xx_unique_x[y_idx].c_str(), cor_);
            }
        }
    }

    std::vector<std::string> ret;
    for (size_t i = 0; i < pcor_unique_x.size(); i++) {
        if (selected[i]) ret.push_back(pcor_unique_x[i]);
    }
    return ret;
}
