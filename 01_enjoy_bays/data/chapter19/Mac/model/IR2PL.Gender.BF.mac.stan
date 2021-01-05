data {
  int<lower=1> J; // number of items
  int<lower=1> I; // number of respondents
  int<lower=1> N; // number of trials (items * respondents)
  int<lower=0, upper=1> g[N]; // genders for trial n
  int<lower=1, upper=J> j[N]; // items for trial n
  int<lower=1, upper=I> ai[N]; // respondents for trial n
  int<lower=0, upper=1> y[N]; // response
}

parameters {
  vector<lower=0>[I] alpha[2]; // discrimination // 識別力
  vector[I] beta[2]; // difficulty // 困難度
  vector[J] theta; // ability or traits // 能力
}

transformed parameters {
  vector[N] mu;
  for (n in 1:N){
    if (g[n]==0) {
      mu[n] = alpha[1,ai[n]] * (theta[j[n]] - beta[1,ai[n]]);
    }else{
      mu[n] = alpha[2,ai[n]] * (theta[j[n]] - beta[2,ai[n]]);
    }
  }
}

model {
  // prior distributions
  for (ii in 1:I) {
    alpha[1,ii] ~ lognormal(0,.25);
    alpha[2,ii] ~ lognormal(0,.25);
    beta[1,ii] ~ normal(0,5);
    beta[2,ii] ~ normal(0,5);
  }
  theta ~ normal(0,1);
  
  // liklihood (Vectorization)
  y ~ bernoulli_logit(mu); // Bernoulli dis. + inverse logit
}

generated quantities {
  vector[N] log_lik;
  int<lower=0,upper=1> y_tilde[N];
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(y[n]| mu[n]);
    y_tilde[n] = bernoulli_rng(inv_logit(mu[n]));
  }
}
