#include <Rcpp.h>
#include <zerg_string.h>
#include <zerg_file.h>

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
std::string fz_now_string() {
    return ztool::now_string();
}

//' now_cob
//'
//' @return int64_t
//' @export
// [[Rcpp::export]]
int64_t fz_now_cob() {
    return ztool::now_cob();
}

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
bool fz_mkdirp(const std::string& path) {
    return ztool::mkdirp(path, 0755) == 0;
}

//' IsFileExisted
//'
//' @param path where to work
//' @return bool success
//' @export
// [[Rcpp::export]]
bool fz_is_file_existed(const std::string& path) {
    return ztool::IsFileExisted(path);
}

//' GetAbsolutePath
//'
//' @param path where to work
//' @return string
//' @export
// [[Rcpp::export]]
std::string fz_get_absolute_path(const std::string& path) {
    return ztool::GetAbsolutePath(path);
}

//' Dirname
//'
//' @param path where to work
//' @return string
//' @export
// [[Rcpp::export]]
std::string fz_dirname(const std::string& path) {
    return ztool::Dirname(path);
}

//' Basename
//'
//' @param path where to work
//' @return string
//' @export
// [[Rcpp::export]]
std::string fz_basename(const std::string& path) {
    return ztool::Basename(path);
}

//' IsDir
//'
//' @param path where to test
//' @return bool success
//' @export
// [[Rcpp::export]]
bool fz_is_dir(const std::string& path) {
    return ztool::IsDir(path);
}

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
