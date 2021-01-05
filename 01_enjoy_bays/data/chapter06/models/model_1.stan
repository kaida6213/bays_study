data {
  int N; //number of subjects
  int r; //nrow of data set
  int C[r]; //Choice Data, delayed option = 1
  int ID[r]; //ID
  real U[r]; //Attractiveness of women (sooner option)
  real A[r]; //Attractiveness of women (later option)
  int Delay[r]; //Delayed days
}
parameters {
  vector<lower=0>[N] beta; //inverse temperature of softmax function
  vector<lower=0, upper=1>[N] gamma; //discount rate of dating and savouring
  vector<lower=0, upper=1>[N] alpha; //savouring rate
  real <lower=0> beta_mu; //hierarchical mean of beta
  real <lower=0> beta_sigma; //hierarchical sd of beta
  real <lower=0, upper = 1> gamma_mu; //hierarchical mean of gamma
  real <lower=0> gamma_sigma; //hierarchical sd of gamma
  real <lower=0, upper=1> alpha_mu; //hierarchical mean of alpha
  real <lower=0> alpha_sigma; //hierarchical sigma of alpha
}

transformed parameters {
  real <lower=0, upper=1> theta[r]; //choice probability of later option
  real V[r]; //the value of delayed option
  for (i in 1:r){
    // Restricted Exponential Savouring model
    V[i] = alpha[ID[i]] * 
           A[i] * 
           pow(gamma[ID[i]], Delay[i]) *
           (1 - pow(gamma[ID[i]], Delay[i])) / (1 - gamma[ID[i]]) + 
           A[i] * pow(gamma[ID[i]], Delay[i]); 
    // softmax function(2 options)
    theta[i] = 1 / (1 + exp(beta[ID[i]] * (U[i] - V[i]))); 
  }
}

model {
  //prior for hierarchical parameter (weakly informative)
  beta_mu ~ normal(0, 10);
  beta_sigma ~ student_t(3, 0, 1);
  gamma_mu ~ normal(0.5, 0.5);
  gamma_sigma ~ student_t(3, 0, 1);
  alpha_mu ~ normal(0.5, 0.5);
  alpha_sigma ~ student_t(3, 0, 1);
  
  //prior for individual parameters
  beta  ~ normal(beta_mu, beta_sigma);
  gamma ~ normal(gamma_mu, gamma_sigma);
  alpha ~ normal(alpha_mu, alpha_sigma);
  
  // choice from bernoulli distribution
  for (i in 1:r)
    C[i] ~ bernoulli(theta[i]); 
}

generated quantities {
  real log_lik[r]; //log-likelihood
  for (i in 1:r)
    log_lik[i] = bernoulli_lpmf(C[i] | theta[i]);
}
