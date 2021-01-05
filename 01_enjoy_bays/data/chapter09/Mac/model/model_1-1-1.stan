data{
  int<lower = 0> N_obs;             //打ち切られていない試行数
  vector[N_obs] Y;                  //打ち切られていない試行の総合得点
  real<lower = max(Y)> U;           //上限打ち切りポイント
}

parameters{
  real mu;
  real<lower = 0> sigma;
  real<lower = U> y_cens;
}

model{
  Y ~ normal(mu, sigma);            //式9.1
  y_cens ~ normal(mu, sigma);       //式9.2
}
