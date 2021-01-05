data{
  int N;
  int y[N];
}

parameters{
  real <lower=0, upper=1> pi;
  ordered [2] theta_tmp;
}

transformed parameters{
  real <lower=0, upper=1> theta[2];
  for(k in 1:2){
    theta[k] = inv_logit(theta_tmp[k]);
  }
}

model{
  for (i in 1:N){
    target += log_sum_exp(
      log(pi) + binomial_lpmf(y[i]|7, theta[1]),
      log1m(pi) + binomial_lpmf(y[i]|7, theta[2])
    );
  }
}
