data {
  int T_obs;                              //�ϑ����ꂽ���s��
  int T_pred;                             //�\�����鎎�s��
  vector[T_obs] Y;                        //�ϑ����ꂽ���s�ɂ����鑍�����_
}

parameters {
  vector<lower=0>[T_obs + T_pred] mu;     //�ϑ����ꂽ���s+�\�����鎎�s�Ԃ�́u�^�̉̏��́v
  real<lower=0> sigma_epsilon;
  real<lower=0> sigma_xi;
}

model {
  for(t in 2 : (T_obs + T_pred))          //��9.6
    mu[t] ~ normal(mu[t-1], sigma_xi);
  
  for(t in 1 : T_obs)                     //��9.7
    Y[t] ~ normal(mu[t], sigma_epsilon);

  mu ~ normal(0, 100);                    //���O���z
  sigma_xi ~ student_t(3, 0, 100);
  sigma_epsilon ~ student_t(3, 0, 100);
}

generated quantities {
  vector[T_pred] y_pred;                  //�\�����z�̐���
  for (t in 1 : T_pred)
    y_pred[t] = normal_rng(mu[T_obs + t], sigma_epsilon);
}
