data {
  int<lower=0> n_all;
  vector[n_all] ww;
  int<lower=0, upper=1> nd[n_all];
}

parameters {
  real c1;
  real c2;
}

model {
  real p[n_all];
  
  // Priors
  c1 ~ normal(-1, 5);
  c2 ~ normal(1, 5);
  
  // Compute probabilities using the logistic function
  for (i in 1:n_all) {
    p[i] = inv_logit(c1 + c2 * ww[i]); // Ensure p[i] is in [0, 1]
  }
  
  // Likelihood for Bernoulli observations
  for (i in 1:n_all) {
    nd[i] ~ bernoulli(p[i]);
  }
}





