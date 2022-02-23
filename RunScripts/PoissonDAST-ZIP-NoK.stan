//
// This Stan program defines the dynamic-additive
// spatiotemporal model of sagebrush cover dynamics.
//
data {
  int<lower=0> n;   // number of observations
  int<lower=0> ny;  // number of years
  int<lower=0> p;   // number of covariates, including intercept

  int<lower=0> iy[n];  // year id for each observation

  int y[n];         // observed counts
  matrix[n,p] X;    // covariates, includeing intercept
  vector[n] Eta;    // spatial field vector subsetted to data

  vector[p] Bm;  // prior means
  vector[p] Bs;  // prior standard deviations
}

parameters {
  real<lower=0, upper=1> theta;
  vector[p] Beta;                // regression coefficients
  vector[ny] gamma;              // temporal random effects
  real<lower=0.000001> sigma_y;  // normal std dev of temporal random effects
}

transformed parameters {
  vector[n] fixed;  // fixed effects response
  vector[n] mu;     // regression mean

  fixed = X*Beta;  // regression fixed effects

  // tack on the random effects
  for(i in 1:n) {
    mu[i] = fixed[i] + gamma[iy[i]] + Eta[i];
  }
}

model {
  // Likelihood --------------------------------------------
  for (i in 1:n) {
    if (y[i] == 0)
      target += log_sum_exp(bernoulli_lpmf(1 | theta),
                            bernoulli_lpmf(0 | theta)
                              + poisson_log_lpmf(y[i] | mu[i]));
    else
      target += bernoulli_lpmf(0 | theta)
                  + poisson_log_lpmf(y[i] | mu[i]);
  }


  // Priors ------------------------------------------------
  theta ~ beta(1, 1);
  // temporal random effects
  gamma ~ normal(0, sigma_y);

  // regression coefficients
  for(i in 1:p) {
    Beta[i] ~ normal(Bm[i], Bs[i]);
  }

  // random effect std devs
  sigma_y ~ cauchy(0, 2.5);
}

