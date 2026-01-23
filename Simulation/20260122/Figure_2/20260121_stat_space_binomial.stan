data {
  int<lower=0> n_all;
  int<lower=0> n_cd;
  int<lower=0> n_d;
  vector[n_d] ww;
  int<lower=0> nd[n_cd];
  int row_d[n_d];
  int row_cd[n_cd];
  int <lower=0> size;
}

parameters {
  vector[n_all] mu;
  //real<lower=0> s1;
  //real<lower=0> s2;
  real c1;
  real <lower=0> c2;
}

model {
  int x1[n_cd];
  int x2[n_d];
  real p[n_all];
  
  // Priors
  mu ~ normal(3, 3);  // Changed from mean 2 to 0 for generality
  //s1 ~ normal(0, 10);
  //s2 ~ normal(0, 10);
  c1 ~ normal(-10, 5);  // Changed mean from -1 to 0 for generality
  c2 ~ normal(3, 5);  // Changed mean from 1 to 0 for generality
  
  // Compute probabilities using the logistic function
  for (i in 3:n_all) {
    mu[i] ~ normal(2 * mu[i - 1] - mu[i - 2], 0.1);
  }
  
  // Likelihood for normal observations
  for (k2 in 1:n_d) {
    x2[k2] = row_d[k2];
    ww[k2] ~ normal(mu[x2[k2]], 0.01);
  }
  
  for (i in 1:n_all) {
    p[i] = inv_logit(c1 + c2 * mu[i]);  // Ensure p[i] is in [0, 1]
  }
  
  // Likelihood for Bernoulli observations
  for (k1 in 1:n_cd) {
    x1[k1] = row_cd[k1];
    nd[k1] ~ binomial(size, p[x1[k1]]);
  }
}




