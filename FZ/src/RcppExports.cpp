// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// fz_split
std::vector<std::string> fz_split(const std::string& str, const std::string& separator);
RcppExport SEXP _FZ_fz_split(SEXP strSEXP, SEXP separatorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string& >::type str(strSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type separator(separatorSEXP);
    rcpp_result_gen = Rcpp::wrap(fz_split(str, separator));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_FZ_fz_split", (DL_FUNC) &_FZ_fz_split, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_FZ(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
