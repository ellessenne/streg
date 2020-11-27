#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.exp_h)]]
arma::vec exp_h(arma::mat const &X, arma::vec const &par) {
  Rcpp::checkUserInterrupt();
  arma::vec out = exp(X * par);
  return(out);
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.exp_surv)]]
arma::vec exp_surv(arma::mat const &X, arma::vec const &t, arma::vec const &par) {
  Rcpp::checkUserInterrupt();
  arma::vec out = exp(-exp(X * par) % t);
  return(out);
}
