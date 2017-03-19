#include <Rcpp.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "xxhash.h"

#include <sstream>

using namespace Rcpp;

template<typename T>
void XXH64_update(XXH64_state_t *state, const T &val)
{
  XXH64_update(state, &val, sizeof(T));
}

void recursive_hash(SEXP v, XXH64_state_t *state)
{
  XXH64_update(state, TYPEOF(v));

  if (TYPEOF(v) == ENVSXP)
  {
    XXH64_update(state, &v, sizeof(v));
  }
  if (TYPEOF(v) == SYMSXP)
  {
    const char *str =  CHAR(PRINTNAME(v));
    XXH64_update(state, str, strlen(str));
  }
  switch (TYPEOF(v)) 
  {
  case CHARSXP:
    XXH64_update(state, CHAR(v), XLENGTH(v)*sizeof(char));
    break;
  case LGLSXP:
    XXH64_update(state, LOGICAL(v), XLENGTH(v)*sizeof(int));
    break;
  case INTSXP:
    XXH64_update(state, INTEGER(v), XLENGTH(v)*sizeof(int));
    break;
  case RAWSXP:
    XXH64_update(state, RAW(v), XLENGTH(v)*sizeof(Rbyte));
    break;
  case REALSXP:
    XXH64_update(state, REAL(v), XLENGTH(v)*sizeof(double));
    break;
  case CPLXSXP:
    XXH64_update(state, COMPLEX(v), XLENGTH(v)*sizeof(Rcomplex));
    break;
    
  case VECSXP: 
  case EXPRSXP:
  {
    for(unsigned int i=0; i < XLENGTH(v); ++i) {
      recursive_hash(VECTOR_ELT(v, i), state);
    }
  }
    break;
  case STRSXP:
  {
    unsigned int i = 0;
    while (i < XLENGTH(v)) {
      recursive_hash(STRING_ELT(v, i), state);
      i++;
    }
  }
    break;
  case LISTSXP: case LANGSXP:
  {
    SEXP lc = v;
    while (lc != R_NilValue) {
      if (TAG(lc) && TAG(lc) != R_NilValue) {
        recursive_hash(TAG(lc), state);
      }
      recursive_hash(CAR(lc), state);
      lc = CDR(lc);
    }
  }
    break;
  case ENVSXP:
    if (FRAME(v) != R_NilValue) {
      recursive_hash(FRAME(v), state);
    }
    recursive_hash(ENCLOS(v), state);
    if (HASHTAB(v) != R_NilValue) {
      recursive_hash(HASHTAB(v), state);
    }
    break;
    
  case CLOSXP:
    recursive_hash(FORMALS(v), state);
    recursive_hash(BODY(v), state);
    recursive_hash(CLOENV(v), state);
    break;
  }
  
  /*if (ATTRIB(v) && ATTRIB(v) != R_NilValue && TYPEOF(v) != CHARSXP) {
    recursive_hash(ATTRIB(v), state);
  }*/
  
}

// [[Rcpp::export]]
std::string call_hash(SEXP s) {
  XXH64_state_t state;
  XXH64_reset(&state, 0);
  
  recursive_hash(s, &state);
  
  unsigned long long val =  XXH64_digest(&state);
  
  std::stringstream ss;
  ss << std::hex << val;
  
  return ss.str();
  
  /*std::string result;
  result.resize(17);
  static char const digits[] = "0123456789ABCDEF";
  for(int i=15; i>=0; --i)
  {
    result[i] = digits[val & (0xF << i)];
  }
  return result;*/
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

call_hash_ <- function(object){
  print(.Internal(inspect(object)))
}

prc <- function(...){
  print(call_hash(sys.call(-1)))
  #call_hash_(sys.call(-1))
}

cmdA <- function(x){
  prc(x)
}

cmdB <- function(x,y){
  prc(x,y)
}

i1 = 0
i2 = cmdA(i1)
i3 = cmdB(i2, 2)
i4 = cmdA(i3)
i5 = cmdA(i1)
i2 = cmdA(i1)

*/
