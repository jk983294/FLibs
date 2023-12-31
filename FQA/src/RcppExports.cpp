// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// select_xx_xy
std::vector<std::string> select_xx_xy(const List& xx_dt, const std::vector<std::string>& pcor_sorted_x, double xx_threshold);
RcppExport SEXP _FQA_select_xx_xy(SEXP xx_dtSEXP, SEXP pcor_sorted_xSEXP, SEXP xx_thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List& >::type xx_dt(xx_dtSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::string>& >::type pcor_sorted_x(pcor_sorted_xSEXP);
    Rcpp::traits::input_parameter< double >::type xx_threshold(xx_thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(select_xx_xy(xx_dt, pcor_sorted_x, xx_threshold));
    return rcpp_result_gen;
END_RCPP
}
// parse_xx
List parse_xx(std::string path, const std::vector<std::string>& fnames);
RcppExport SEXP _FQA_parse_xx(SEXP pathSEXP, SEXP fnamesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type path(pathSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::string>& >::type fnames(fnamesSEXP);
    rcpp_result_gen = Rcpp::wrap(parse_xx(path, fnames));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_FQA_select_xx_xy", (DL_FUNC) &_FQA_select_xx_xy, 3},
    {"_FQA_parse_xx", (DL_FUNC) &_FQA_parse_xx, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_FQA(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
