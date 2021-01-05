data {
  int T_obs;                              //観測された試行数
  int T_pred;                             //予測する試行数
  vector[T_obs] Y;                        //観測された試行における総合得点
}

parameters {
  vector<lower=0>[T_obs + T_pred] mu;     //観測された試行+予測する試行ぶんの「真の歌唱力」
  real<lower=0> sigma_epsilon;
  real<lower=0> sigma_xi;
}

model {
  for(t in 2 : (T_obs + T_pred))          //式9.6
    mu[t] ~ normal(mu[t-1], sigma_xi);
  
  for(t in 1 : T_obs)                     //式9.7
    Y[t] ~ normal(mu[t], sigma_epsilon);

  mu ~ normal(0, 100);                    //事前分布
  sigma_xi ~ student_t(3, 0, 100);
  sigma_epsilon ~ student_t(3, 0, 100);
}

generated quantities {
  vector[T_pred] y_pred;                  //予測分布の生成
  for (t in 1 : T_pred)
    y_pred[t] = normal_rng(mu[T_obs + t], sigma_epsilon);
}
