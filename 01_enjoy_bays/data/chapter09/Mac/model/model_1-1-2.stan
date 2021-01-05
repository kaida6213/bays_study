data{
  int<lower = 1> N;                                     //総試行数
  int<lower = 0> N_cens;                                //打ち切られた試行数
  real<lower = 0> Y[N];                                 //全試行の総合得点
  int censored[N];                                      //打ち切りの有無（1:打ち切り有り、0:打ち切り無し）
  real<lower = 0> U;                                    //上限打ち切りポイント
}

parameters{
  real mu;
  real<lower = 0> sigma;
}

model{
  for (n in 1:N){
    if(censored[n] == 0)                                //打ち切りではない試行
      Y[n] ~ normal(mu, sigma);                         //式9.3
    else                                                //打ち切られた試行
      target += N_cens * normal_lccdf(U | mu, sigma);   //式9.4
  }
}
