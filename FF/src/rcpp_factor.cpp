
#include <Rcpp.h>
#include <math_stats.h>
#include <math_vector.h>

using namespace Rcpp;

//' as_int_group, if nan/inf, then group return -1
//'
//' @param group vector of double
//' @export
// [[Rcpp::export]]
std::vector<int> as_int_group(const std::vector<double>& group) {
    std::vector<int> ret;
    ret.resize(group.size(), -1);

    for (size_t i = 0; i < group.size(); i++) {
        if (std::isfinite(group[i])) {
            ret[i] = std::round(group[i]);
        }
    }
    return ret;
}

//' quantile_cut, if nan/inf, factor value will be mapped to 0
//'
//' @param data vector of double
//' @param numQtls number of quantiles
//' @export
// [[Rcpp::export]]
std::vector<int> qcut(const std::vector<double>& data, int numQtls) {
    std::vector<std::pair<double, size_t>> sortedData(data.size());
    for (size_t i = 0; i < data.size(); i++) {
        if (std::isfinite(data[i]))
            sortedData[i] = {data[i], i};
        else
            sortedData[i] = {0., i};
    }

    std::sort(sortedData.begin(), sortedData.end(), [](auto& l, auto& r) { return l.first < r.first; });

    std::vector<int> discretized(data.size());

    // Calculate the size of each bucket
    int bucketSize = std::ceil(static_cast<double>(data.size()) / numQtls);

    for (size_t i = 0; i < data.size(); ++i) {
        int bucket = i / bucketSize;
        discretized[sortedData[i].second] = bucket + 1;
    }

    return discretized;
}

//' fill_zero, if nan/inf, then return 0
//'
//' @param v vector of double
//' @export
// [[Rcpp::export]]
std::vector<double> fill_zero(const std::vector<double>& v) {
    std::vector<double> ret = v;
    for (size_t i = 0; i < ret.size(); i++) {
        if (!std::isfinite(ret[i])) {
            ret[i] = 0;
        }
    }
    return ret;
}

//' to_weights
//'
//' @param v vector of double
//' @param demean if vector demean
//' @param equal_weight if vector equal_weight
//' @export
// [[Rcpp::export]]
std::vector<double> to_weights(const std::vector<double>& v, bool demean, bool equal_weight) {
    std::vector<double> ret = fill_zero(v);
    if (equal_weight) {
        size_t pos_cnt = 0, neg_cnt = 0;
        if (demean) {
            // top assets positive weights, bottom ones negative
            double md = ornate::median(ret);
            ornate::vs_minus_inplace(ret, md);
            for (size_t i = 0; i < ret.size(); i++) {
                if (ret[i] > 0.) {
                    ret[i] = 1.;
                    pos_cnt++;
                } else if (ret[i] < 0.) {
                    ret[i] = -1.;
                    neg_cnt++;
                }
            }
        }

        if (demean) {
            // positive weights must equal negative weights
            for (size_t i = 0; i < ret.size(); i++) {
                if (ret[i] > 0. && pos_cnt > 1) {
                    ret[i] = 1. / pos_cnt;
                } else if (ret[i] < 0. && neg_cnt > 1) {
                    ret[i] = -1. / neg_cnt;
                }
            }
        }
    } else if (demean) {
        double m_ = ornate::mean(ret);
        ornate::vs_minus_inplace(ret, m_);
    }
    double sum_of_abs =
        std::accumulate(ret.begin(), ret.end(), 0., [](double sum, double val) { return sum + std::abs(val); });
    if (sum_of_abs > 1e-6) {
        for (double& i : ret) i = i / sum_of_abs;
    }
    return ret;
}

//' as_int_group, if nan/inf, then group return -1
//'
//' @param group vector of int
//' @param name "d" for day, "m" for month, "y" for year
//' @export
// [[Rcpp::export]]
std::vector<int> time_group(const std::vector<int>& date, const std::string& name) {
    std::vector<int> ret = date;
    if (name == "m")
        for (auto& d : ret) d /= 100;
    else if (name == "y")
        for (auto& d : ret) d /= 10000;
    return ret;
}
