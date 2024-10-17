
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

std::vector<std::string> get_column_names(const List& dt) {
    std::vector<std::string> ret;
    CharacterVector xcols = dt.names();
    for (int i = 0; i < xcols.size(); i++) {
        String str_(xcols[i]);
        ret.push_back(str_.get_cstring());
    }
    return ret;
}

/**
 * size of (set1 - set2)
 */
template <typename T>
size_t set_diff_cnt(const std::set<T>& set1, const std::set<T>& set2) {
    std::vector<T> difference;
    std::set_difference(set1.begin(), set1.end(),
                        set2.begin(), set2.end(),
                        std::back_inserter(difference));
    return difference.size();
}

//' calc_qtl_turnover
//' analyzing the turnover properties of a factor
//'
//' @param dt data table like struct
//' @param qtl_name default "f_qtl"
//' @param shift period default 1
//' @export
// [[Rcpp::export]]
List calc_qtl_turnover(const List& dt, const std::string& qtl_name, int shift) {
    std::vector<std::string> xcols = get_column_names(dt);
    std::string ukey_col_name = "ukey";
    std::string tick_col_name = "ticktime";
    std::string date_col_name = "DataDate";
    if (!dt.containsElementNamed(ukey_col_name.c_str())) {
        printf("dt has no %s column\n", ukey_col_name.c_str());
        return List::create();
    }
    if (!dt.containsElementNamed(tick_col_name.c_str())) {
        printf("dt has no %s column\n", tick_col_name.c_str());
        return List::create();
    }
    if (!dt.containsElementNamed(date_col_name.c_str())) {
        printf("dt has no %s column\n", date_col_name.c_str());
        return List::create();
    }
    if (!dt.containsElementNamed(qtl_name.c_str())) {
        printf("dt has no %s column\n", qtl_name.c_str());
        return List::create();
    }

    IntegerVector ukeys = dt[ukey_col_name];
    IntegerVector ticks = dt[tick_col_name];
    IntegerVector dates = dt[date_col_name];
    IntegerVector qtls = dt[qtl_name];
    IntegerVector unique_ukeys = Rcpp::unique(ukeys).sort();
    IntegerVector unique_ticks = Rcpp::unique(ticks).sort();
    IntegerVector unique_dates = Rcpp::unique(dates).sort();
    IntegerVector unique_qtls = Rcpp::unique(qtls).sort();
    long nrow = ukeys.size();
    int max_ii = unique_ukeys.size();
    int max_di = unique_dates.size();
    int max_ti = unique_ticks.size();
    int max_qi = unique_qtls.size();

    std::unordered_map<int, int> date2di;
    std::unordered_map<int, int> tick2ti;
    std::unordered_map<int, int> ukey2ii;
    std::unordered_map<int, int> qtl2qi;

    for (int i = 0; i < max_di; i++) date2di[unique_dates[i]] = i;
    for (int i = 0; i < max_ti; i++) tick2ti[unique_ticks[i]] = i;
    for (int i = 0; i < max_ii; i++) ukey2ii[unique_ukeys[i]] = i;
    for (int i = 0; i < max_qi; i++) qtl2qi[unique_qtls[i]] = i;

    std::vector<std::vector<std::vector<std::set<int>>>> d2t2q(max_di);
    for (auto& t2q : d2t2q) {
        t2q.resize(max_ti, std::vector<std::set<int>>(max_qi));
    }

    for (long i = 0; i < nrow; i++) {
        int di = date2di[dates[i]];
        int ti = tick2ti[ticks[i]];
        int qi = qtl2qi[qtls[i]];
        d2t2q[di][ti][qi].insert(ukeys[i]);
    }
    
    std::vector<double> tvs(max_di * max_ti * max_qi);
    std::vector<int> o_dates(max_di * max_ti * max_qi);
    std::vector<int> o_ticks(max_di * max_ti * max_qi);
    std::vector<int> o_qtls(max_di * max_ti * max_qi);

    for (int di = 0; di < max_di; di++) {
        for (int ti = 0; ti < max_ti; ti++) {
            int tick_offset = di * max_ti + ti;
            for (int qi = 0; qi < max_qi; qi++) {
                long offset = max_ti * max_qi * di;
                o_dates[offset + qi] = unique_dates[di];
                o_ticks[offset + qi] = unique_ticks[ti];
                o_qtls[offset + qi] = unique_qtls[qi];
                if (tick_offset - shift >= 0) {
                    int tick_offset_ = tick_offset - shift;
                    int target_di = tick_offset_ / max_ti;
                    int target_ti = tick_offset_ % max_ti;
                    const auto& from = d2t2q[target_di][target_ti][qi];
                    const auto& to = d2t2q[di][ti][qi];
                    size_t total_cnt = to.size();
                    if (total_cnt > 0) {
                        size_t dif_cnt = set_diff_cnt(to, from);
                        tvs[offset + qi] = (double)dif_cnt / total_cnt;
                    } else {
                        tvs[offset + qi] = NAN;
                    }
                } else {
                    tvs[offset + qi] = NAN;
                }
            }
        }
    }

    return List::create(Named(date_col_name) = o_dates, Named(tick_col_name) = o_ticks, 
        Named(qtl_name) = o_qtls, Named("tvs") = tvs);
}

//' calc_rank_acf
//' Computes autocorrelation of factor ranks
//'
//' @param dt data table like struct
//' @param f_name default "factor"
//' @param shift period default 1
//' @export
// [[Rcpp::export]]
List calc_rank_acf(const List& dt, const std::string& f_name, int shift) {
    std::vector<std::string> xcols = get_column_names(dt);
    std::string ukey_col_name = "ukey";
    std::string tick_col_name = "ticktime";
    std::string date_col_name = "DataDate";
    if (!dt.containsElementNamed(ukey_col_name.c_str())) {
        printf("dt has no %s column\n", ukey_col_name.c_str());
        return List::create();
    }
    if (!dt.containsElementNamed(tick_col_name.c_str())) {
        printf("dt has no %s column\n", tick_col_name.c_str());
        return List::create();
    }
    if (!dt.containsElementNamed(date_col_name.c_str())) {
        printf("dt has no %s column\n", date_col_name.c_str());
        return List::create();
    }
    if (!dt.containsElementNamed(f_name.c_str())) {
        printf("dt has no %s column\n", f_name.c_str());
        return List::create();
    }

    IntegerVector ukeys = dt[ukey_col_name];
    IntegerVector ticks = dt[tick_col_name];
    IntegerVector dates = dt[date_col_name];
    DoubleVector factors = dt[f_name];
    IntegerVector unique_ukeys = Rcpp::unique(ukeys).sort();
    IntegerVector unique_ticks = Rcpp::unique(ticks).sort();
    IntegerVector unique_dates = Rcpp::unique(dates).sort();
    long nrow = ukeys.size();
    int max_ii = unique_ukeys.size();
    int max_di = unique_dates.size();
    int max_ti = unique_ticks.size();

    std::unordered_map<int, int> date2di;
    std::unordered_map<int, int> tick2ti;
    std::unordered_map<int, int> ukey2ii;

    for (int i = 0; i < max_di; i++) date2di[unique_dates[i]] = i;
    for (int i = 0; i < max_ti; i++) tick2ti[unique_ticks[i]] = i;
    for (int i = 0; i < max_ii; i++) ukey2ii[unique_ukeys[i]] = i;

    std::vector<std::vector<std::vector<double>>> d2t2f(max_di);
    for (auto& t2f : d2t2f) {
        t2f.resize(max_ti, std::vector<double>(max_ii, NAN));
    }

    for (long i = 0; i < nrow; i++) {
        int di = date2di[dates[i]];
        int ti = tick2ti[ticks[i]];
        int ii = ukey2ii[ukeys[i]];
        d2t2f[di][ti][ii] = factors[i];
    }
    
    std::vector<double> acfs(max_di * max_ti);
    std::vector<int> o_dates(max_di * max_ti);
    std::vector<int> o_ticks(max_di * max_ti);

    for (int di = 0; di < max_di; di++) {
        for (int ti = 0; ti < max_ti; ti++) {
            ornate::rank(d2t2f[di][ti]);

            int tick_offset = di * max_ti + ti;
            long offset = max_ti * di;
            o_dates[offset + ti] = unique_dates[di];
            o_ticks[offset + ti] = unique_ticks[ti];
            if (tick_offset - shift >= 0) {
                int tick_offset_ = tick_offset - shift;
                int target_di = tick_offset_ / max_ti;
                int target_ti = tick_offset_ % max_ti;
                const auto& from = d2t2f[target_di][target_ti];
                const auto& to = d2t2f[di][ti];
                
                acfs[offset + ti] = ornate::corr(from, to);
            } else {
                acfs[offset + ti] = NAN;
            }
        }
    }

    return List::create(Named(date_col_name) = o_dates, Named(tick_col_name) = o_ticks, 
        Named("acf") = acfs);
}
