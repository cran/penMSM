#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericMatrix fishercpp(NumericMatrix & Xcpp, NumericVector & mucpp){
  int ncol = Xcpp.ncol();
  int nrow = Xcpp.nrow();
  NumericMatrix A(nrow, ncol);
  // A(1, 1) = y[1] * X[1, 1]
  // A(2, 1) = y[2] * X[2, 1]
  // A(2, 3) = y[2] * X[2, 3]
  for (int i=0; i<nrow; i++) {
    for (int j=0; j<ncol; j++) {
      A(i,j) = mucpp[i] * Xcpp(i,j);
    }
  }
  NumericMatrix B(ncol, ncol);
  for (int k=0; k<ncol; k++) {
    for (int l=0; l<ncol; l++) {
      for (int m=0; m<nrow; m++) {
        B(k,l) += Xcpp(m,k) * A(m,l);
      }
    }
  }
  return B;
}
