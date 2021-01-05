data{
  int<lower=1> N; // sample size.
  int<lower=1> P; // number of teams.
  int<lower=1> K; // dimensions.
  real R;         // parameter of uniform distribution.
  matrix[N,P] Y;  // data matrix.
}

parameters{
  vector<lower=-R,upper=R>[K] theta[N];
  vector<lower=-R,upper=R>[K] delta[P];
  real<lower=0,upper=1> gamma[N];
  real<lower=-2,upper=2> alpha;
  real<lower=0> beta;
  real<lower=0> sigma[P];
}

model{
  matrix[N,P] mu;
  for(n in 1:N){
    for(p in 1:P){
      mu[n,p] = alpha - beta*dot_self(theta[n]-delta[p])^0.5;
    }
  }
  for(n in 1:N){
    for(p in 1:P){
      if(Y[n,p]==0.5){
        target += log_sum_exp(gamma[n] ,
                              (1-gamma[n]) + normal_lpdf(0.5|mu[n,p],sigma[p]));
      }else if(Y[n,p]==0){
        target += (1-gamma[n]) + normal_lcdf(0|mu[n,p],sigma[p]);
      }else if(Y[n,p]==1){
        target += (1-gamma[n]) + normal_lccdf(1|mu[n,p],sigma[p]);
      }else{
        target += (1-gamma[n]) + normal_lpdf(Y[n,p]|mu[n,p],sigma[p]);
      }
    } 
  }
}
