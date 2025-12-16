data {
  int<lower=0> n_all;
  int<lower=0> n_cd;
  int<lower=0> n_d;
  vector[n_d] ww;
  int<lower=0> nd[n_cd];
  int<lower=0> n_count[n_cd];
  int row_d[n_d];
  int row_cd[n_cd];
}

parameters {
  vector<lower=-4, upper=4>[n_all] mu_raw;
  real<lower=-4, upper=4> s1_raw;
  real<lower=-4, upper=4> s2_raw;
}

transformed parameters{
      vector[n_all] mu;
  for (i in 1:n_all) {
    mu[i] = 3.5 + 1.5 * mu_raw[i];
  }
    real s1;
    s1 = exp(s1_raw - 2);
    
    real s2;
    s2 = exp(s2_raw - 2);
  
    real c1 = -22.85;
  
    real c2 = 5.88;

}


model {
  // Priors
  mu_raw ~ normal(0, 1);
  s1_raw ~ normal(0, 1);
  s2_raw ~ normal(0, 1);
  
  // Compute probabilities using the logistic function
  for (i in 3:n_all) {
    mu[i] ~ normal(2 * mu[i - 1] - mu[i - 2], s1);
  }
  
  // Likelihood for normal observations
  for (k2 in 1:n_d) {
    x2[k2] = row_d[k2];
    ww[k2] ~ normal(mu[x2[k2]], s2);
  }
  
  for (i in 1:n_all) {
    p[i] = inv_logit(c1 + c2 * mu[i]);  // Ensure p[i] is in [0, 1]
  }
  
  // Likelihood for Bernoulli observations
  for (k1 in 1:n_cd) {
    x1[k1] = row_cd[k1];
    nd[k1] ~ binomial(n_count[k1], p[x1[k1]]);
  }
}







