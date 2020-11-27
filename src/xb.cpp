#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.xb)]]
arma::vec xb(arma::mat const &X, arma::vec const &par) {
  Rcpp::checkUserInterrupt();
  int n = X.n_cols;
  arma::vec beta = par.head(n);
  arma::vec out = (X * beta);
  return(out);
}
