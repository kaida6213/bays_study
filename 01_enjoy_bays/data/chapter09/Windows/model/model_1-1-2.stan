data{
  int<lower = 1> N;                                     //�����s��
  int<lower = 0> N_cens;                                //�ł��؂�ꂽ���s��
  real<lower = 0> Y[N];                                 //�S���s�̑������_
  int censored[N];                                      //�ł��؂�̗L���i1:�ł��؂�L��A0:�ł��؂薳���j
  real<lower = 0> U;                                    //����ł��؂�|�C���g
}

parameters{
  real mu;
  real<lower = 0> sigma;
}

model{
  for (n in 1:N){
    if(censored[n] == 0)                                //�ł��؂�ł͂Ȃ����s
      Y[n] ~ normal(mu, sigma);                         //��9.3
    else                                                //�ł��؂�ꂽ���s
      target += N_cens * normal_lccdf(U | mu, sigma);   //��9.4
  }
}
