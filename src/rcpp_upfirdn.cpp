#include <Rcpp.h>
#include "Resampler.h"

using namespace Rcpp;


RcppExport SEXP upfirdn__new(SEXP upRate_, SEXP downRate_, SEXP coefs_) {
  int upRate = as<int>(upRate_), downRate = as<int>(downRate_);
  
  std::vector<double> coefs = as<std::vector<double> >(coefs_);
  
  Rcpp::XPtr<Resampler<double,double,double> > ptr( new Resampler<double,double,double>( upRate, downRate, coefs.data(), coefs.size() ), true );
  return ptr; 
}

RcppExport SEXP upfirdn__apply_multichannel( SEXP xp, SEXP data_ ) {
  Rcpp::XPtr<Resampler<double,double,double> > ptr(xp);
  
  std::vector<double> data = as<std::vector<double> >(data_);
  
  std::vector<double> out;
  out.resize(ptr->neededOutCount(data.size()));
  
  ptr->apply(data.data(), data.size(), out.data(), out.size());
  
  return wrap(out); 
}
