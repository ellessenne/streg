/// @file ll_exp.hpp

#ifndef ll_exp_hpp
#define ll_exp_hpp

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

/// Negative log-likelihood of the normal distribution.
template<class Type>
Type ll_exp(objective_function<Type>* obj) {
  DATA_VECTOR(time);
  DATA_VECTOR(status);
  DATA_MATRIX(data);
  PARAMETER_VECTOR(beta);
  vector<Type> ll = status * (data * beta) - exp(data * beta) * time + status * log(time);
  return -sum(ll);
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this

#endif
