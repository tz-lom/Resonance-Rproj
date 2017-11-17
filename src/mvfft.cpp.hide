#include <Rcpp.h>
using namespace Rcpp;

#include <fftw3.h>
/*
// [[Rcpp::export]]
ComplexMatrix threadedMvFFTw(NumericMatrix data)
{
  
  ComplexMatrix result(data.nrow(), data.ncol());
  
  Rcout << fftw_init_threads();
  fftw_plan_with_nthreads(8);
  
  int rank = 1;
  int n[] = {data.nrow()};
  int howmany = data.ncol();
  int idist = n[0];
  int odist = idist;
  int istride = 1;
  int ostride = 1; 
  int *inembed = n, *onembed = n;
  
  fftw_plan plan = fftw_plan_many_dft_r2c(
    rank, n, 
    howmany, REAL((SEXP)data), 
    inembed, istride,
    idist, (fftw_complex*)COMPLEX((SEXP)result),
    onembed,  ostride, odist,
    FFTW_PRESERVE_INPUT|FFTW_EXHAUSTIVE
  );
  
  fftw_execute(plan);
  fftw_destroy_plan(plan);
  
  return result;
}
*/

#include "tp/threadpool.hpp"

class Precomputed
{
public:
  boost::threadpool::pool pool;
  int columns;
  int rows;
  fftw_plan plan;
  double *in;
  fftw_complex *out;
  
  Precomputed(int rows, int columns):
    rows(rows),
    columns(columns)
  {
    int cores = 8;
    pool = boost::threadpool::pool(8);
    
    in = fftw_alloc_real(rows);
    out = fftw_alloc_complex(rows);
    plan = fftw_plan_dft_r2c_1d(rows, in, out, FFTW_DESTROY_INPUT | FFTW_EXHAUSTIVE);
  }
  
  ~Precomputed()
  {
    fftw_free(in);
    fftw_free(out);
    fftw_destroy_plan(plan);
  }
};

// [[Rcpp::export]]
SEXP prepareMvFFT(int rows, int columns)
{
  Precomputed *pc = new Precomputed(rows, columns);
  return XPtr<Precomputed>(pc, false );
}

void executeFFT(fftw_plan plan, int rows, double *in, fftw_complex *out)
{
  //double *fIn = (double*)fftw_malloc(sizeof(double) * rows);
  fftw_complex *fOut = (fftw_complex*)fftw_malloc(sizeof(fftw_complex) * rows);
  
  //memcpy(fIn, in, rows*sizeof(double));
  
  //Rcout << fftw_alignment_of(fIn) << " " << fftw_alignment_of((double*)fOut) << "\n";
  
  //fftw_plan pl = fftw_plan_dft_r2c_1d(rows, fIn, fOut, FFTW_ESTIMATE);
  //fftw_execute(pl);
  //fftw_destroy_plan(pl);
  
  fftw_execute_dft_r2c(plan, in, fOut);
  
  memcpy(out, fOut, (rows/2+1)*sizeof(fftw_complex));
  
  //fftw_free(fIn);
  fftw_free(fOut);
}

// [[Rcpp::export]]
ComplexMatrix performMvFFT(SEXP xpc, NumericMatrix data)
{
  XPtr<Precomputed> pc(xpc);
  int nrow = data.nrow();
  
  ComplexMatrix result(nrow, data.ncol());
  
  double *in = REAL((SEXP)data);
  fftw_complex *out = (fftw_complex*)COMPLEX((SEXP)result);
  
  
  //fftw_plan pl = fftw_plan_dft_r2c_1d(pc->rows, in, out, FFTW_ESTIMATE);
  //fftw_execute(pl);
  //fftw_destroy_plan(pl);
  
  for(int i=0; i<pc->columns; ++i,in+=nrow,out+=nrow)
  {
    pc->pool.schedule(boost::bind(executeFFT, pc->plan, pc->rows, in, out));
    //Rcout << fftw_alignment_of(pc->in) << " " << fftw_alignment_of((double*)pc->out) << "\n";
    //executeFFT(pc->plan, pc->rows, in, out);
  }
 // Rcout << "out " << out[0][0] << " " << out[0][1] << "\n";
//  Rcout << result;
  pc->pool.wait();
  return result;
}
