#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix forceCopy(NumericMatrix m)
{
  return clone(m);
}

// [[Rcpp::export]]
void shiftRows(NumericMatrix m, int shift)
{
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
  
  int rows = m.nrow()-shift;
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

// [[Rcpp::export]]
void replace_columns_block(NumericMatrix dest, int destRow, int destCol, NumericMatrix src)
{
  int srcR = src.nrow();
  int srcC = src.ncol();
  int destR = dest.nrow();
  int destC = dest.ncol();
  
  int newR = 0, newC = 0;
  
  if(srcR+destRow > destR) newR = srcR+destRow;
  if(srcC+destCol > destC) newC = srcC+destCol;
  
  if(newC || newR)
  {
    std::cout << "resize";
    // resise destination
    if(!newC) newC = destC;
    if(!newR) newR = destR;
    NumericMatrix N(newC, newR);
    //N(Range(0, destR-1), Range(0, destC-1)). dest;
    for(int r=0; r<destR; ++r)
      for(int c=0; c<destC; ++c)
        N(r, c) = dest(r, c);
    dest = N;
  }
  
  for(int dr=destRow, sr=0; sr<srcR; ++sr,++dr)
  {
    for(int sc=0, dc=destC; sc<srcC; ++sc,++dc)
    {
      
      dest(dr, dc) = src(sr, sc);
      std::cout << dest(dr,dc) << std::endl;
    }
  }
  
  //return dest;
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