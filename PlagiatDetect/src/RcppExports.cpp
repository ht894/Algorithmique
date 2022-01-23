// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// rcpp_hello_world
List rcpp_hello_world();
RcppExport SEXP _PlagiatDetect_rcpp_hello_world() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcpp_hello_world());
    return rcpp_result_gen;
END_RCPP
}
// cpp_to_lowercase
char cpp_to_lowercase(char c);
RcppExport SEXP _PlagiatDetect_cpp_to_lowercase(SEXP cSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< char >::type c(cSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_to_lowercase(c));
    return rcpp_result_gen;
END_RCPP
}
// cpp_tokenizer
std::vector<std::string> cpp_tokenizer(std::string texte);
RcppExport SEXP _PlagiatDetect_cpp_tokenizer(SEXP texteSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type texte(texteSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_tokenizer(texte));
    return rcpp_result_gen;
END_RCPP
}
// cpp_get_length
int cpp_get_length(std::string texte);
RcppExport SEXP _PlagiatDetect_cpp_get_length(SEXP texteSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type texte(texteSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_get_length(texte));
    return rcpp_result_gen;
END_RCPP
}
// cpp_Levenshtein
int cpp_Levenshtein(std::string str1, std::string str2);
RcppExport SEXP _PlagiatDetect_cpp_Levenshtein(SEXP str1SEXP, SEXP str2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type str1(str1SEXP);
    Rcpp::traits::input_parameter< std::string >::type str2(str2SEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_Levenshtein(str1, str2));
    return rcpp_result_gen;
END_RCPP
}
// cpp_plagiat_Levenshtein
int cpp_plagiat_Levenshtein(std::string texte1, std::string texte2);
RcppExport SEXP _PlagiatDetect_cpp_plagiat_Levenshtein(SEXP texte1SEXP, SEXP texte2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type texte1(texte1SEXP);
    Rcpp::traits::input_parameter< std::string >::type texte2(texte2SEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_plagiat_Levenshtein(texte1, texte2));
    return rcpp_result_gen;
END_RCPP
}
// Scores_matrixCpp
NumericMatrix Scores_matrixCpp(std::string str1, std::string str2, int gap_cost);
RcppExport SEXP _PlagiatDetect_Scores_matrixCpp(SEXP str1SEXP, SEXP str2SEXP, SEXP gap_costSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type str1(str1SEXP);
    Rcpp::traits::input_parameter< std::string >::type str2(str2SEXP);
    Rcpp::traits::input_parameter< int >::type gap_cost(gap_costSEXP);
    rcpp_result_gen = Rcpp::wrap(Scores_matrixCpp(str1, str2, gap_cost));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_PlagiatDetect_rcpp_hello_world", (DL_FUNC) &_PlagiatDetect_rcpp_hello_world, 0},
    {"_PlagiatDetect_cpp_to_lowercase", (DL_FUNC) &_PlagiatDetect_cpp_to_lowercase, 1},
    {"_PlagiatDetect_cpp_tokenizer", (DL_FUNC) &_PlagiatDetect_cpp_tokenizer, 1},
    {"_PlagiatDetect_cpp_get_length", (DL_FUNC) &_PlagiatDetect_cpp_get_length, 1},
    {"_PlagiatDetect_cpp_Levenshtein", (DL_FUNC) &_PlagiatDetect_cpp_Levenshtein, 2},
    {"_PlagiatDetect_cpp_plagiat_Levenshtein", (DL_FUNC) &_PlagiatDetect_cpp_plagiat_Levenshtein, 2},
    {"_PlagiatDetect_Scores_matrixCpp", (DL_FUNC) &_PlagiatDetect_Scores_matrixCpp, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_PlagiatDetect(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
