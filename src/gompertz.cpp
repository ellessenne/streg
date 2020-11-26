#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.gom_h)]]
arma::vec gom_h(arma::mat const &X, arma::vec const &t, arma::vec const &par) {
  Rcpp::checkUserInterrupt();
  unsigned int n = X.n_cols;
  double gamma = par(n); // indexing starts at zero here, hence this works
  arma::vec beta = par.head(n);
  arma::vec lambda = exp(X * beta);
  arma::vec out = lambda % exp(gamma * t);
  return(out);
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.gom_surv)]]
arma::vec gom_surv(arma::mat const &X, arma::vec const &t, arma::vec const &par) {
  Rcpp::checkUserInterrupt();
  unsigned int n = X.n_cols;
  double gamma = par(n); // indexing starts at zero here, hence this works
  arma::vec beta = par.head(n);
  arma::vec lambda = exp(X * beta);
  arma::vec out = exp((-lambda / gamma) % (exp(gamma * t) - 1.0));
  return(out);
}
