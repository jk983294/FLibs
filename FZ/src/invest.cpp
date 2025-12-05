#include <Rcpp.h>
#include <zerg/string.h>
#include <zerg/io/file.h>
#include <data/MkdFut.h>
#include <data/misc.h>

using namespace Rcpp;

//' get_cn_fut_code
//'
//' @rdname fz-functions
//' @param ukeys ukey to look up
//' @export
// [[Rcpp::export]]
std::vector<std::string> get_cn_fut_code(const std::vector<int>& ukeys, const std::string& file) {
    std::vector<std::string> rets(ukeys.size());
    mkd::CnFutUkeyMetaMap umm;
    umm.read(file);
    for (size_t i = 0; i < ukeys.size(); i++) {
        rets[i] = umm.get_pdt(ukeys[i]);
    }
    return rets;
}


//' all_cn_fut_code
//'
//' @rdname fz-functions
//' @param ukeys ukey to look up
//' @export
// [[Rcpp::export]]
List all_cn_fut_code(const std::string& file) {
    std::vector<std::string> pdts, exches;
    std::vector<int> ukeys;
    std::vector<double> price_ticks, multipliers;
    mkd::CnFutUkeyMetaMap umm;
    umm.read(file);
    for (auto& meta : umm.metas) {
        ukeys.push_back(meta.ukey);
        pdts.push_back(meta.pdt);
        exches.push_back(meta.exch);
        price_ticks.push_back(meta.price_tick);
        multipliers.push_back(meta.multiplier);
    }
    return List::create( _("ukey") = ukeys, _("pdt") = pdts, _("exch") = exches, _("price_tick") = price_ticks, _("multiplier") = multipliers);
}

//' GetUkey
//'
//' @rdname fz-functions
//' @param str ukey string
//' @return ukey int
//' @export
// [[Rcpp::export]]
int32_t GetUkey(const std::string& str) {
    if (str.find('.') != std::string::npos) {
        return mkd::convertChWindCodeToUkey(str);
    } else if (str.find('@') != std::string::npos) {
        return mkd::convertInstrToUkey(str);
    } else {
        return mkd::convertChCodeToUkey(str);
    }
}

//' GetUkey
//'
//' @rdname fz-functions
//' @param str ukey string
//' @param exch string
//' @return ukey int
//' @export
// [[Rcpp::export]]
int32_t GetUkey2(const std::string& str, const std::string& exch) {
    return mkd::convertInstrToUkey(str, exch);
}