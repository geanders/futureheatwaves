// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_futureheatwaves_RCPPEXPORTS_H_GEN_
#define RCPP_futureheatwaves_RCPPEXPORTS_H_GEN_

#include <Rcpp.h>

namespace futureheatwaves {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("futureheatwaves", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("futureheatwaves", "futureheatwaves_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in futureheatwaves");
            }
        }
    }

    inline Rcpp::DataFrame IDHeatwavesCPP(int heatwaveLength, Rcpp::NumericVector tempsExceedingCutoff) {
        typedef SEXP(*Ptr_IDHeatwavesCPP)(SEXP,SEXP);
        static Ptr_IDHeatwavesCPP p_IDHeatwavesCPP = NULL;
        if (p_IDHeatwavesCPP == NULL) {
            validateSignature("Rcpp::DataFrame(*IDHeatwavesCPP)(int,Rcpp::NumericVector)");
            p_IDHeatwavesCPP = (Ptr_IDHeatwavesCPP)R_GetCCallable("futureheatwaves", "futureheatwaves_IDHeatwavesCPP");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_IDHeatwavesCPP(Rcpp::wrap(heatwaveLength), Rcpp::wrap(tempsExceedingCutoff));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::DataFrame >(rcpp_result_gen);
    }

    inline void storeHeatwaveEntry(int index, int hwSize, int hwCounter, std::vector<int>& hw, std::vector<int>& hwNumber) {
        typedef SEXP(*Ptr_storeHeatwaveEntry)(SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_storeHeatwaveEntry p_storeHeatwaveEntry = NULL;
        if (p_storeHeatwaveEntry == NULL) {
            validateSignature("void(*storeHeatwaveEntry)(int,int,int,std::vector<int>&,std::vector<int>&)");
            p_storeHeatwaveEntry = (Ptr_storeHeatwaveEntry)R_GetCCallable("futureheatwaves", "futureheatwaves_storeHeatwaveEntry");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_storeHeatwaveEntry(Rcpp::wrap(index), Rcpp::wrap(hwSize), Rcpp::wrap(hwCounter), Rcpp::wrap(hw), Rcpp::wrap(hwNumber));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
    }

    inline void storeZeroes(int index, int potentialHeatwave, std::vector<int>& hw, std::vector<int>& hwNumber) {
        typedef SEXP(*Ptr_storeZeroes)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_storeZeroes p_storeZeroes = NULL;
        if (p_storeZeroes == NULL) {
            validateSignature("void(*storeZeroes)(int,int,std::vector<int>&,std::vector<int>&)");
            p_storeZeroes = (Ptr_storeZeroes)R_GetCCallable("futureheatwaves", "futureheatwaves_storeZeroes");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_storeZeroes(Rcpp::wrap(index), Rcpp::wrap(potentialHeatwave), Rcpp::wrap(hw), Rcpp::wrap(hwNumber));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
    }

}

#endif // RCPP_futureheatwaves_RCPPEXPORTS_H_GEN_
