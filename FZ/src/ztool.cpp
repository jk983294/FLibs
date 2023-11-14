#include <Rcpp.h>
#include <zerg_cn_fut.h>
#include <zerg_file.h>
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

//' SplitInstrumentID
//'
//' @param str string to split
//' @return std::vector of string
//' @export
// [[Rcpp::export]]
std::vector<std::string> fz_split_instrument_id(const std::string& str) {
    auto item = ztool::SplitInstrumentID(str);
    return {item.first, item.second};
}

//' now_string
//'
//' @return string
//' @export
// [[Rcpp::export]]
std::string fz_now_string() { return ztool::now_string(); }

//' now_cob
//'
//' @return int64_t
//' @export
// [[Rcpp::export]]
int64_t fz_now_cob() { return ztool::now_cob(); }

//' HumanReadableMillisecond like 20us
//'
//' @param interval_string what to work
//' @return uint64_t
//' @export
// [[Rcpp::export]]
uint64_t fz_HumanReadableMillisecond(const std::string& interval_string) {
    return ztool::HumanReadableMillisecond(interval_string);
}

//' replace_time_placeholder
//'
//' @param str what to work
//' @param date which date to use
//' @return string
//' @export
// [[Rcpp::export]]
std::string fz_replace_time_placeholder(const std::string& str, int date) {
    return ztool::replace_time_placeholder(str, date);
}

//' mkdirp
//'
//' @param path where to work
//' @return bool success
//' @export
// [[Rcpp::export]]
bool fz_mkdirp(const std::string& path) { return ztool::mkdirp(path, 0755) == 0; }

//' IsFileExisted
//'
//' @param path where to work
//' @return bool success
//' @export
// [[Rcpp::export]]
bool fz_is_file_existed(const std::string& path) { return ztool::IsFileExisted(path); }

//' GetAbsolutePath
//'
//' @param path where to work
//' @return string
//' @export
// [[Rcpp::export]]
std::string fz_get_absolute_path(const std::string& path) { return ztool::GetAbsolutePath(path); }

//' Dirname
//'
//' @param path where to work
//' @return string
//' @export
// [[Rcpp::export]]
std::string fz_dirname(const std::string& path) { return ztool::Dirname(path); }

//' Basename
//'
//' @param path where to work
//' @return string
//' @export
// [[Rcpp::export]]
std::string fz_basename(const std::string& path) { return ztool::Basename(path); }

//' IsDir
//'
//' @param path where to test
//' @return bool success
//' @export
// [[Rcpp::export]]
bool fz_is_dir(const std::string& path) { return ztool::IsDir(path); }

//' read_trading_days
//'
//' @param path where to read
//' @return vector of dates
//' @export
// [[Rcpp::export]]
std::vector<int> fz_read_trading_days(const std::string& path) {
    std::vector<int> ret;
    ztool::read_trading_days(path, ret);
    return ret;
}

//' path wildcard like /tmp/*.csv
//'
//' @param path where to search
//' @return map <matched, path>
//' @export
// [[Rcpp::export]]
std::unordered_map<std::string, std::string> fz_path_wildcard(const std::string& path) {
    return ztool::path_wildcard(path);
}

//' str_expand2
//'
//' @param expr expr to expand
//' @param names names to expand
//' @param values values to expand
//' @return vector of strs
//' @export
// [[Rcpp::export]]
std::vector<std::string> str_expand2(const std::string& expr, const std::vector<std::string>& names,
                                     const std::vector<std::vector<std::string>>& values) {
    size_t max_size = 1;
    for (auto& v : values) max_size *= (v.size() == 0 ? 1 : v.size());
    std::vector<std::string> tmp1;
    std::vector<std::string> tmp2;
    tmp1.reserve(max_size);
    tmp2.reserve(max_size);
    tmp1.push_back(expr);
    for (size_t i = 0; i < names.size(); i++) {
        std::string ph = "{" + names[i] + "}";
        if (expr.find(ph) == std::string::npos) continue;
        for (auto& str : tmp1) {
            const std::vector<std::string>* _vals = &values.front();
            if (i < values.size()) _vals = &values[i];
            for (auto& val : *_vals) {
                tmp2.push_back(ztool::ReplaceAllCopy(str, ph, val));
            }
        }
        tmp1.swap(tmp2);
        tmp2.clear();
    }
    {
        std::string ph = "{N}";
        for (size_t i = 0; i < tmp1.size(); i++) {
            tmp1[i] = ztool::ReplaceAllCopy(tmp1[i], ph, std::to_string(i));
        }
    }
    return tmp1;
}

//' expr_split
//'
//' @param exprs expr to split
//' @param N N to split
//' @param config_path meta config file
//' @export
// [[Rcpp::export]]
std::vector<std::string> expr_split(const std::vector<std::string>& exprs, int N, const std::string& config_path) {
    std::vector<std::string> rets;
    long each_n = std::lround(std::ceil((double)exprs.size() / N));
    if (each_n <= 0) return rets;
    bool meta_exist = ztool::IsFileExisted(config_path);
    string meta_text;
    if (meta_exist) {
        std::ifstream t(config_path);
        std::string str((std::istreambuf_iterator<char>(t)), std::istreambuf_iterator<char>());
        meta_text = str;
    }
    for (int i = 0; i < N; i++) {
        auto path_ = config_path + ".expr.split." + std::to_string(i);
        std::ofstream ofs(path_, std::ofstream::out | std::ofstream::trunc);
        size_t left = each_n * i;
        size_t right = left + each_n;
        if (left >= exprs.size()) break;
        if (right > exprs.size()) right = exprs.size();
        for (size_t j = left; j < right; j++) {
            ofs << exprs[j] << std::endl;
        }
        ofs.close();
        rets.push_back(path_);

        if (meta_exist) {
            string text1 = meta_text;
            ztool::ReplaceAll(text1, "${CONFIG_PATH}", path_);
            ztool::ReplaceAll(text1, "${N}", std::to_string(i));
            std::fstream fs;
            string ct_path_split = config_path + ".split." + std::to_string(i);
            fs.open(ct_path_split, std::fstream::out);
            fs << text1;
            fs.close();
            printf("write config %s\n", ct_path_split.c_str());
        }
    }
    return rets;
}

//' expr_split_to_dt
//'
//' @param exprs expr to split
//' @param delimiter delimiter to split
//' @param is_first true first, false last
//' @export
// [[Rcpp::export]]
List expr_split_to_dt(const std::vector<std::string>& exprs, char delimiter, bool is_first = true) {
    std::vector<std::string> f0(exprs.size());
    std::vector<std::string> f1(exprs.size());
    for (size_t i = 0; i < exprs.size(); i++) {
        size_t end = exprs[i].find_first_of(delimiter);
        if (not is_first) end = exprs[i].find_last_of(delimiter);
        if (end != std::string::npos) {
            f0[i] = exprs[i].substr(0, end);
            f1[i] = exprs[i].substr(end + 1);
        }
    }
    return List::create(_("f0") = f0, _("f1") = f1, _("exprs") = exprs);
}