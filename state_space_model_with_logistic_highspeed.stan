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
  real<lower=-4, upper=4> c1_raw;
  real<lower=-4, upper=4> c2_raw;
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
  
    real c1;
     c1 = -22 + 5*c1_raw;
  
    real c2;
    c2 = exp(1+c2_raw);

}


model {
  // Priors
  mu_raw ~ normal(0, 1);
  s1_raw ~ normal(0, 1);
  s2_raw ~ normal(0, 1);
  c1_raw ~ normal(0, 1);
  c2_raw ~ normal(0, 1);
  
  // Autoregressive prior for mu
    mu[3:n_all] ~ normal(2 * mu[2:n_all-1] - mu[1:n_all-2], s1);

  
  // Likelihood for normal observations
    for (k in 1:n_d) {
      ww[k] ~ normal(mu[row_d[k]], s2);
    } 
  
  // Compute probabilities using the logistic function and likelihood for Bernoulli observations
    vector[n_cd] p;
    for (k in 1:n_cd) {
      p[k] = inv_logit(c1 + c2 * mu[row_cd[k]]);
      nd[k] ~ binomial(n_count[k], p[k]); 
    } 
}





