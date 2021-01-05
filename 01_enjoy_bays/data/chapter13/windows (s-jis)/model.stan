data{
  int N; // �T���v���T�C�Y
  int G; // �W�c�̐�
  int<lower=0> post[N]; // �㔼�̍�Ɨ�
  real pre[N]; // �O���̍�Ɨ� (�l���x��)
  real preg[N]; // �O���̍�Ɨ� (�W�c���x��)
  real peerN[N]; // �W�c�̐l��
  int<lower=0, upper=1> cond[N]; // ��������̗L��
  int<lower=1, upper=G> group[N]; // �W�c�̔ԍ�
}

parameters{
  vector[2] gamma_0; // gamma_00, gamma_10�̕��ϒl
  vector[2] b[G]; // gamma_00, gamma_01
  vector[3] gamma0_; // gamma_01, gamma_02, gamma_03
  vector<lower=0>[2] tau; // tau_00, tau_10
  real<lower=0> sigma; // �㔼�̍�Ɨʂ̕W���΍�
}

transformed parameters{
  real mu[N]; // �㔼�̍�Ɨʂ̕��ϒl
  for(i in 1:N) // ��(6)
    mu[i] = b[group[i]][1] + b[group[i]][2]*pre[i] +
                      gamma0_[1]*preg[i] + gamma0_[2]*peerN[i] +
                      gamma0_[3]*cond[i];
}

model{
  gamma_0 ~ normal(0, 100); // gamma_00, gamma_10�̕��ϒl�̎��O���z
  tau ~ cauchy(0,100); //�@tau_00, tau_10�̎��O���z
  gamma0_ ~ normal(0, 100); // gamma_01, gamma_02, gamma_03�̎��O���z
  sigma ~ cauchy(0, 100); // �㔼�̍�Ɨʂ̕W���΍��̎��O���z
  
  for(i in 1:G)
    b[i] ~ normal(gamma_0, tau); // ��(5)
  
  for(i in 1:N)
    post[i] ~ normal(mu[i],sigma); // ��(1)
}

generated quantities{
  real es; // ���ʗ�
  real rate; // �ؕЂ̕��ϒl�ɑ΂��镽�ϒl���̊���
  es = gamma0_[3]/sigma;
  rate = gamma0_[3]/gamma_0[1];
}
