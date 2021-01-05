data{
  int N; // サンプルサイズ
  int G; // 集団の数
  int<lower=0> post[N]; // 後半の作業量
  real pre[N]; // 前半の作業量 (個人レベル)
  real preg[N]; // 前半の作業量 (集団レベル)
  real peerN[N]; // 集団の人数
  int<lower=0, upper=1> cond[N]; // 実験操作の有無
  int<lower=1, upper=G> group[N]; // 集団の番号
}

parameters{
  vector[2] gamma_0; // gamma_00, gamma_10の平均値
  vector[2] b[G]; // gamma_00, gamma_01
  vector[3] gamma0_; // gamma_01, gamma_02, gamma_03
  vector<lower=0>[2] tau; // tau_00, tau_10
  real<lower=0> sigma; // 後半の作業量の標準偏差
}

transformed parameters{
  real mu[N]; // 後半の作業量の平均値
  for(i in 1:N) // 式(6)
    mu[i] = b[group[i]][1] + b[group[i]][2]*pre[i] +
                      gamma0_[1]*preg[i] + gamma0_[2]*peerN[i] +
                      gamma0_[3]*cond[i];
}

model{
  gamma_0 ~ normal(0, 100); // gamma_00, gamma_10の平均値の事前分布
  tau ~ cauchy(0,100); //　tau_00, tau_10の事前分布
  gamma0_ ~ normal(0, 100); // gamma_01, gamma_02, gamma_03の事前分布
  sigma ~ cauchy(0, 100); // 後半の作業量の標準偏差の事前分布
  
  for(i in 1:G)
    b[i] ~ normal(gamma_0, tau); // 式(5)
  
  for(i in 1:N)
    post[i] ~ normal(mu[i],sigma); // 式(1)
}

generated quantities{
  real es; // 効果量
  real rate; // 切片の平均値に対する平均値差の割合
  es = gamma0_[3]/sigma;
  rate = gamma0_[3]/gamma_0[1];
}
