data{
  int N;
  int y[N];
}

parameters{
  real mu;
  real<lower=0> sigma;
  real r[N];
}

model{
  for (i in 1:N){
    y[i] ~ binomial_logit(7, r[i]);
    r[i] ~ normal(mu, sigma);
  }
}

generated quantities{
  real theta;
  theta=inv_logit(normal_rng(mu,sigma));
}
