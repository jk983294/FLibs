#include <Rcpp.h>
#include <vector>
#include <set>
#include <zerg/time/bizday.h>
#include <zerg/time/bizday.h>

using namespace Rcpp;

RCPP_MODULE(FZ) {
    Rcpp::class_<zerg::BizDayConfig>("BizDayConfig")
        .constructor("Default constructor for BizDayConfig")
        .constructor<std::string>("Constructor for BizDayConfig that takes a biz day file")
        .method("init", &zerg::BizDayConfig::init, "Initialize the BizDayConfig from a file")
        .method("checkBizDay", &zerg::BizDayConfig::checkBizDay, "Check if a date is a business day")
        .method("bizDayRange", &zerg::BizDayConfig::bizDayRange, "Get business days in a range")
        .method("lowerBound", &zerg::BizDayConfig::lowerBound, "Get the lower bound business day for a given date")
        .method("lowerBoundIndex", &zerg::BizDayConfig::lowerBoundIndex, "Get the index of the lower bound business day")
        .method("next_day", &zerg::BizDayConfig::next, "Get the next business day from a given date")
        .method("prev_day", &zerg::BizDayConfig::prev, "Get the previous business day from a given date")
        .method("offset", &zerg::BizDayConfig::offset, "Get business day with an offset from a given date")
        .method("first_day", &zerg::BizDayConfig::first_day, "Get the first business day in the configuration")
        .method("last_day", &zerg::BizDayConfig::last_day, "Get the last business day in the configuration")
        ;
}