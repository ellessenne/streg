/// @file ll_gom.hpp

#ifndef ll_gom_hpp
#define ll_gom_hpp

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

/// Negative log-likelihood of the normal distribution.
template<class Type>
Type ll_gom(objective_function<Type>* obj) {
  DATA_VECTOR(time);
  DATA_VECTOR(status);
  DATA_MATRIX(data);
  PARAMETER_VECTOR(beta);
  PARAMETER(gamma);
  vector<Type> ll = status * (data * beta + gamma * time) - exp(data * beta) * pow(gamma, -1.0) * (exp(gamma * time) - 1) + status * log(time);
  return -sum(ll);
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this

#endif
