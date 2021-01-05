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
  vector<lower=0>[N] beta; //inverse temperature of softmaxfunction
  vector<lower=0, upper=1>[N] gamma_d; //discount rate of dating
  vector<lower=0, upper=1>[N] gamma_s; //discount rate of savouring
  vector<lower=0, upper=1>[N] alpha; //savouring rate
  real <lower=0> beta_mu; //hierarchical mean of beta
  real <lower=0> beta_sigma; //hierarchical sd of beta
  real <lower=0, upper=1> gamma_d_mu; //hierarchical mean of gamma_d 
  real <lower=0> gamma_d_sigma; //hierarchical sd of gamma_d
  real <lower=0, upper=1> gamma_s_mu; //hierarchical mean of gamma_s
  real <lower=0> gamma_s_sigma; //hierarchical sigma of gamma_s
  real <lower=0, upper=1> alpha_mu; //hierarchical mean of alpha
  real <lower=0> alpha_sigma; //hierarchical sd of alpha
}

transformed parameters {
  real <lower=0, upper=1> theta[r]; //choice probability of later option
  real V[r]; //the value of delayed option
  for (i in 1:r){
    // Discount Exponential Savouring model
    V[i] = alpha[ID[i]] *
      A[i] *
      pow(gamma_s[ID[i]], Delay[i]) *
      (1 - pow(gamma_d[ID[i]], Delay[i])) /
      (1 - gamma_d[ID[i]]) + A[i] *
      pow(gamma_d[ID[i]], Delay[i]);
    //softmax function(2 option)
    theta[i] = 1 / (1 + exp(beta[ID[i]] * (U[i]-V[i]))); 
  }
}

model {
  //prior for hierarchical parameter (weakly informative)
  beta_mu ~ normal(0, 10);
  beta_sigma ~ student_t(3, 0, 1);
  gamma_d_mu ~ normal(0.5, 0.5);
  gamma_d_sigma ~ student_t(3, 0, 1);
  gamma_s_mu ~ normal(0.5, 0.5);
  gamma_s_sigma ~ student_t(3, 0, 1);
  alpha_mu ~ normal(0.5, 0.5);
  alpha_sigma ~ student_t(3, 0, 1);
  
  //prior for individual parameters
  beta  ~ normal(beta_mu, beta_sigma);
  gamma_d ~ normal(gamma_d_mu, gamma_d_sigma);
  gamma_s ~ normal(gamma_s_mu, gamma_s_sigma);
  alpha ~ normal(alpha_mu, alpha_sigma);

  //choice from bernoulli distribution
  for (i in 1:r)
    C[i] ~ bernoulli(theta[i]); 
}

generated quantities {
  real log_lik[r]; //log-likelihood
  for (i in 1:r)
    log_lik[i] = bernoulli_lpmf(C[i] | theta[i]);
}
