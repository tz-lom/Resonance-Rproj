#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix forceCopy(NumericMatrix m)
{
  return clone(m);
}

template<typename Type>
void doShiftRows(SEXP matr, int shift)
{
  Type m = as<Type>(matr);
  if(shift==0) return;
  if( (shift>0 && shift>m.nrow()) ||
      (shift<0 && -shift>m.nrow())) stop("Shift exceeds matrix dimension");
  int cols = m.ncol();
  
  if(shift>0)
  {
    for(int row=m.nrow()-shift; row>=0; --row)
    {
      for(int col=cols-1; col>=0; --col)
      {
        m(row+shift,col) = m(row,col);
      }
    }
  }
  else
  {
    int rows = m.nrow()+shift;
    for(int row=0; row< rows; ++row)
    {
      for(int col=cols-1; col>=0; --col)
      {
        m(row,col) = m(row-shift,col);
      }
    }
  }
}


// [[Rcpp::export]]
void shiftRows(SEXP m, int shift)
{
  switch(TYPEOF(m))
  {
  case INTSXP:
    doShiftRows<IntegerMatrix>(m, shift);
    break;
  case REALSXP:
    doShiftRows<NumericMatrix>(m, shift);
    break;
  case STRSXP:
    doShiftRows<StringMatrix>(m, shift);
    break;
  case LGLSXP:
    doShiftRows<LogicalMatrix>(m, shift);
    break;
  default:
    stop("Unhandled type of matrix");
  }
}

// [[Rcpp::export]]
void pushRows_bottom(NumericMatrix m, NumericMatrix p)
{
  int cols = m.ncol();
  if(p.ncol()!=cols) return;
  
  int shift = p.nrow();
  
  int rows = m.nrow()-shift;
  if(rows>=0)
  {
    for(int row=0; row< rows; ++row)
    {
      for(int col=cols-1; col>=0; --col)
      {
        m(row,col) = m(row+shift,col);
      }
    }
    for(int dst=m.nrow()-1, src=shift-1; src>=0; --src, --dst )
    {
      for(int col=cols-1; col>=0; --col)
      {
        m(dst, col) = p(src, col);
      }
    }
  }
  else
  {
    for(int dst=0, src=-rows; src<shift; ++src, ++dst)
    {
      for(int col=cols-1; col>=0; --col)
      {
        m(dst, col) = p(src, col);
      }
    }
  }
}

// [[Rcpp::export]]
void pushRows_top(NumericMatrix m, NumericMatrix p)
{
  int cols = m.ncol();
  if(p.ncol()!=cols) return;
  
  int shift = p.nrow();
  
  for(int row=m.nrow()-shift; row>=0; --row)
  {
    for(int col=cols-1; col>=0; --col)
    {
      m(row+shift,col) = m(row,col);
    }
  }
  for(int row=shift-1; row>=0; --row )
  {
    for(int col=cols-1; col>=0; --col)
    {
      m(row, col) = p(row, col);
    }
  }
}

// [[Rcpp::export]]
void push_slice_rows_back(NumericMatrix dest, NumericMatrix src, int slicePos, int sliceSize)
{
  int srcR = src.nrow();
  int destR = dest.nrow();
  int cols = dest.ncol();
  if(src.ncol()!=cols) stop("number of columns must be equal");
  if(slicePos<0) stop("slicePos incorrect");
  if(sliceSize<0) stop("sliceSize incorrect");
  if(srcR<slicePos || srcR< slicePos+sliceSize) stop("Source smaller that slice");
  if(destR<sliceSize) stop("sliceSize bigger than dest");
  if(sliceSize==0) return;
  
  if(destR> sliceSize)
  {
    int rows = destR-sliceSize;
    for(int row=0; row< rows; ++row)
    {
      for(int col=cols-1; col>=0; --col)
      {
        dest(row,col) = dest(row+sliceSize,col);
      }
    }
    for(int dr=destR-1, sr=slicePos+sliceSize-1; dr>=rows; --sr, --dr )
    {
      for(int col=cols-1; col>=0; --col)
      {
        dest(dr, col) = src(sr, col);
      }
    }
  }
  else
  {
    for(int sr=slicePos+sliceSize-1, dr=sliceSize-1; dr>=0; --dr,--sr )
    {
      for(int col=cols-1; col>=0; --col)
      {
        dest(dr, col) = src(sr, col);
      }
    }
  }
}

// [[Rcpp::export]]
void rowsCopy(NumericMatrix dest, int destBegin, NumericMatrix src, int srcBegin, int srcSize)
{
  int srcR = src.nrow();
  int destR = dest.nrow();
  int cols = dest.ncol();
  if(srcSize==-1) srcSize = srcR;
  if(src.ncol()!=cols) stop("number of columns must be equal");
  if(srcSize<0) stop("srcSize can't be less than 0");
  if(destBegin<0) stop("destBegin incorrect");
  if(srcBegin<0) stop("srcBegin incorrect");
  if(destBegin+srcSize>destR) stop("too little dest");
  if(srcBegin+srcSize>srcR) stop("too big srcSize");
  if(srcSize==0) return;
  
  int limit = srcBegin+srcSize;
  for(int dr=destBegin, sr=srcBegin; sr<limit; ++sr,++dr)
  {
    for(int col=cols-1; col>=0; --col)
      {
        dest(dr, col) = src(sr, col);
      }
  }
}

template <typename Type>
void do_replace_columns_block(SEXP dest_sexp, int destRow, int destCol, NumericMatrix src)
{
  Type dest = as<Type>(dest_sexp);
  int srcR = src.nrow();
  int srcC = src.ncol();
  int destR = dest.nrow();
  int destC = dest.ncol();
  
  if((srcR+destRow > destR) || (srcC+destCol > destC)) stop("target matrix is not big enough");
  
  
  for(int sc=0, dc=destCol; sc<srcC; ++sc,++dc)
  {
    for(int dr=destRow, sr=0; sr<srcR; ++sr,++dr)
    {
      dest(dr, dc) = src(sr, sc);
      //std::cout << dest(dr,dc) << " " << sr << " " << sc<< " " << dr<< " " << dc << std::endl;
    }
  }
}

// [[Rcpp::export]]
void replace_columns_block(SEXP m, int destRow, int destCol, NumericMatrix src)
{
  switch(TYPEOF(m))
  {
  case INTSXP:
    do_replace_columns_block<IntegerMatrix>(m, destRow, destCol, src);
    break;
  case REALSXP:
    do_replace_columns_block<NumericMatrix>(m, destRow, destCol, src);
    break;
  case STRSXP:
    do_replace_columns_block<StringMatrix>(m, destRow, destCol, src);
    break;
  case LGLSXP:
    do_replace_columns_block<LogicalMatrix>(m, destRow, destCol, src);
    break;
  default:
    stop("Unhandled type of matrix");
  }
}

// [[Rcpp::export]]
void copyColumns(NumericMatrix dest, NumericMatrix src, IntegerVector cols)
{
  if(dest.ncol()!=cols.length()) stop("dest must have columns equal to cols length");
  if(dest.nrow()!=src.nrow()) stop("src and dest must have equal number of rows");
  int nRow = dest.nrow()-1;
  
  for(int col=cols.length()-1; col>=0; --col)
  {
    int targ = cols[col]-1;
    for(int row=nRow; row>=0; --row)
    {
      dest(row, col) = src(row, targ);
    }
  }
}

/* [Rcpp::export]]
SEXP DataBlock(SEXP something, NumericVector timestamp)
{
  if(className.attr("class")=="integer64")
  {
    
  }
  else
  {
    
  }
  return something;
}*/
