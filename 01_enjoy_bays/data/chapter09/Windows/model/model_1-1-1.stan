data{
  int<lower = 0> N_obs;             //�ł��؂��Ă��Ȃ����s��
  vector[N_obs] Y;                  //�ł��؂��Ă��Ȃ����s�̑������_
  real<lower = max(Y)> U;           //����ł��؂�|�C���g
}

parameters{
  real mu;
  real<lower = 0> sigma;
  real<lower = U> y_cens;
}

model{
  Y ~ normal(mu, sigma);            //��9.1
  y_cens ~ normal(mu, sigma);       //��9.2
}
