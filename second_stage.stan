data {
  int<lower=0> N;             // Number of observations
  vector[N] distance;         // Independent variable
  int<lower=0, upper=1> indicator[N];  // Dependent variable, binary indicator
  real<lower=0> mean;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'slope' and 'intercept'.
parameters {
  real slope;
  real<lower=0, upper=1> intercept;
}

// The model to be estimated
model {
  intercept ~ normal(mean, 0.0001);
  slope ~ exponential(20);
  // likelihood
  
  for ( i in 1:N) {
    indicator[i] ~ binomial_logit(1, slope * (33-distance[i]) + intercept);
  }
}