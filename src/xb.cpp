#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.xb)]]
arma::vec xb(arma::mat const &X, arma::vec const &par) {
  Rcpp::checkUserInterrupt();
  arma::vec out = (X * par);
  return(out);
}
