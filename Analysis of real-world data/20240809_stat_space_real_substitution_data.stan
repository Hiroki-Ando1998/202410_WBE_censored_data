//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> n_all;
  int<lower=0> n_d;
  vector [n_d] ww;
  int row_d [n_d];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[n_all] mu;
  real<lower=0> s1;
  real<lower=0> s2;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  int x [n_d];
  mu ~ normal(3, 3);
  s1 ~ normal(0, 1.5);
  s2 ~ normal(0, 1.5);
  for(i in 3:n_all){
  mu[i] ~ normal(2*mu[i-1] - mu[i-2], s1);
}
  for(k in 1:n_d){
    x[k] = row_d[k];
  ww[k] ~ normal(mu[x[k]], s2);
}
}






