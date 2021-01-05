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
  vector<lower=0>[I] alpha; // discrimination // 識別力
  vector[I] beta; // difficulty // 困難度
  vector[J] theta; // ability or traits // 能力
}

transformed parameters {
  vector[N] mu;
  for (n in 1:N){
    mu[n] = alpha[ai[n]] * (theta[j[n]] - beta[ai[n]]);
  }
}

model {
  // prior distributions
  target += lognormal_lpdf(alpha| 0, .25);
  target += normal_lpdf(beta| 0, 5);
  target += normal_lpdf(theta| 0, 1);

  // liklihood (Vectorization)
  target += bernoulli_logit_lpmf(y|mu); // Bernoulli dis. + inverse logit
}

generated quantities {
  vector[N] log_lik;
  int<lower=0,upper=1> y_tilde[N];
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(y[n]| mu[n]);
    y_tilde[n] = bernoulli_rng(inv_logit(mu[n]));
  }
}
