#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.wei_h)]]
arma::vec wei_h(arma::mat const &X, arma::vec const &t, arma::vec const &par) {
  Rcpp::checkUserInterrupt();
  uint n = X.n_cols;
  double ln_p = par(n); // indexing starts at zero here, hence this works
  double p = exp(ln_p);
  arma::vec beta = par.head(n);
  arma::vec out = (exp(X * beta) % pow(t, p - 1.0)) * p;
  return(out);
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.wei_surv)]]
arma::vec wei_surv(arma::mat const &X, arma::vec const &t, arma::vec const &par) {
  Rcpp::checkUserInterrupt();
  uint n = X.n_cols;
  double ln_p = par(n); // indexing starts at zero here, hence this works
  double p = exp(ln_p);
  arma::vec beta = par.head(n);
  arma::vec out = exp(-exp(X * beta) % pow(t, p));
  return(out);
}
