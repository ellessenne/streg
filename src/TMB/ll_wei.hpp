/// @file ll_wei.hpp

#ifndef ll_wei_hpp
#define ll_wei_hpp

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

/// Negative log-likelihood of the normal distribution.
template<class Type>
Type ll_wei(objective_function<Type>* obj) {
  DATA_VECTOR(time);
  DATA_VECTOR(status);
  DATA_MATRIX(data);
  PARAMETER_VECTOR(beta);
  PARAMETER(logp);
  Type p = exp(logp);
  vector<Type> ll = status * (logp + data * beta + (p - 1) * log(time)) - exp(data * beta) * pow(time, p) + status * log(time);
  return -sum(ll);
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this

#endif
