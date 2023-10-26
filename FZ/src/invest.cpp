#include <Rcpp.h>
#include <zerg_string.h>
#include <zerg_file.h>
#include <zerg_cn_fut.h>

using namespace Rcpp;

//' get_cn_fut_code
//'
//' @param ukeys ukey to look up
//' @export
// [[Rcpp::export]]
std::vector<std::string> get_cn_fut_code(const std::vector<int>& ukeys) {
    std::vector<std::string> rets(ukeys.size());
    ztool::CnFutUkeyMetaMap umm;
    umm.read();
    for (size_t i = 0; i < ukeys.size(); i++) {
        rets[i] = umm.get_pdt(ukeys[i]);
    }
    return rets;
}


//' all_cn_fut_code
//'
//' @param ukeys ukey to look up
//' @export
// [[Rcpp::export]]
List all_cn_fut_code() {
    std::vector<std::string> pdts, exches;
    std::vector<int> ukeys;
    ztool::CnFutUkeyMetaMap umm;
    umm.read();
    for (auto& meta : umm.metas) {
        ukeys.push_back(meta.ukey);
        pdts.push_back(meta.pdt);
        exches.push_back(meta.exch);
    }
    return List::create( _("ukey") = ukeys, _("pdt") = pdts, _("exch") = exches);
}