//階層ベイズモデルex-Gaussianフィッティング

data{
  int N;
  int S;
  real RT[N];
  int <lower=1, upper=S> SUBID[N];
  int <lower=1, upper=2> CONDID[N];
}

parameters{
  //mu
  vector<lower=0>[2] mu;
  real<lower=0> mu_search_sd;
  real<lower=0> mu_mem_sd;
  real<lower=-1,upper=1> mu_rho;
  vector<lower=0>[2] mu_ind[S];
  
  //sigma
  vector<lower=0>[2] sigma;
  real<lower=0> sigma_search_sd;
  real<lower=0> sigma_mem_sd;
  real<lower=-1,upper=1> sigma_rho;
  vector<lower=0>[2] sigma_ind[S];
  
  //lambda
  vector<lower=0>[2] lambda; 
  real<lower=0> lambda_search_sd;
  real<lower=0> lambda_mem_sd;
  real<lower=-1,upper=1> lambda_rho;
  vector<lower=0>[2] lambda_ind[S];
  //vector<lower=0>[2] tau_ind[S];
}

transformed parameters {
 matrix[2,2] mu_cov_mx;
 matrix[2,2] lambda_cov_mx;
 matrix[2,2] sigma_cov_mx;
 
 //muの個人差のためのマトリクス
 mu_cov_mx[1,1] = mu_search_sd^2;
 mu_cov_mx[2,1] = mu_search_sd*mu_mem_sd*mu_rho;
 mu_cov_mx[1,2] = mu_search_sd*mu_mem_sd*mu_rho;
 mu_cov_mx[2,2] = mu_mem_sd^2;
 
 //sigmaの個人差のためのマトリクス
 sigma_cov_mx[1,1] = sigma_search_sd^2;
 sigma_cov_mx[2,1] = sigma_search_sd*sigma_mem_sd*sigma_rho;
 sigma_cov_mx[1,2] = sigma_search_sd*sigma_mem_sd*sigma_rho;
 sigma_cov_mx[2,2] = sigma_mem_sd^2;
 
 //lambdaの個人差のためのマトリクス
 lambda_cov_mx[1,1] = lambda_search_sd^2;
 lambda_cov_mx[2,1] = lambda_search_sd*lambda_mem_sd*lambda_rho;
 lambda_cov_mx[1,2] = lambda_search_sd*lambda_mem_sd*lambda_rho;
 lambda_cov_mx[2,2] = lambda_mem_sd^2;
 
}

model{
  //個人の各条件のパラメタ（μ、σ、λ）を2変量正規分布からサンプリング
  for(i in 1 : S){
      mu_ind[i] ~ multi_normal(mu, mu_cov_mx);
      sigma_ind[i] ~ multi_normal(sigma, sigma_cov_mx);
      lambda_ind[i] ~ multi_normal(lambda, lambda_cov_mx);
    }
    
  //個人のパラメタを用いて、ex-Gaussian分布から反応時間をサンプリング
  for(i in 1 : N)
    RT[i] ~ exp_mod_normal(mu_ind[SUBID[i],CONDID[i]],sigma_ind[SUBID[i],CONDID[i]],lambda_ind[SUBID[i],CONDID[i]]);
}

generated quantities{
  real mu_diff;
  real sigma_diff;
  real lambda_diff;
  
  mu_diff = mu[2]-mu[1];
  sigma_diff = sigma[2] - sigma[1];
  lambda_diff = lambda[2]-lambda[1];
  
}


