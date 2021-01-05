data {
  int N;  //nrow of data set
  int NP;  //number of participants
  real<lower=0, upper=180> Angle[N]; //rotational angles of letters
  real <lower=0> RT[N];  //response times
  int Par[N];  //participants
}

parameters {
  //global parameters over means (across participants)
  real<lower=200> m_base; //mean of base parameters
  real<lower=0> m_rate; //mean of rate parameters
  real<lower=-2,upper=2> m_log_expo; //mean of log-transformed expo parameters
  real<lower=0, upper=60> m_sigma; //mean of sigma parameters 
  real<lower=-6.5,upper=-4.0> m_log_lambda; //mean of log-transformed lambda parameters
  
  //global parameters over SDs (across participants)
  real<lower=0> s_base;  //SD of base parameters
  real<lower=0> s_rate;  //SD of rate parameters
  real<lower=0, upper=3> s_log_expo;  //SD of log-transformed expo parameters
  real<lower=0> s_sigma;  //SD of sigma parameters
  real<lower=0, upper=1.5> s_log_lambda;  //SD of log-transformed lambda parameters
  
  //local parameters (for each participant)
  real<lower=0> base[NP];  //base parameters
  real<lower=0> rate[NP];  //rate parameters
  real<lower=-3,upper=3> log_expo[NP];  //log-transformed expo parameters
  real<lower=0> sigma[NP];  //sigma parameters
  real<lower=-6.5,upper=-4.0> log_lambda[NP];  //log-transformed lambda parameters
  
  //correlations between local parameters
  corr_matrix[5] rho;  //correlation matrix of local parameters over participants
}

transformed parameters{
  real expo[NP]; //expo parameters
  real lambda[NP];  //lambda parameters
  vector[5] m;  //vector of global parameters over means
  vector<lower=0>[5] s;  //vector of global parameters over SDs
  vector[5] local[NP];  //vectors of local parameters for each participant
  cov_matrix[5] cov;  //covariance matrix of local parameters over participants
  
  for (np in 1:NP) {
    expo[np] = exp(log_expo[np]);
    lambda[np] = exp(log_lambda[np]);
  }
  
  m[1] = m_base;
  m[2] = m_rate;
  m[3] = m_log_expo;
  m[4] = m_sigma;
  m[5] = m_log_lambda;
  
  s[1] = s_base;
  s[2] = s_rate;
  s[3] = s_log_expo;
  s[4] = s_sigma;
  s[5] = s_log_lambda;
  
  local[,1] = base;
  local[,2] = rate;
  local[,3] = log_expo;
  local[,4] = sigma;
  local[,5] = log_lambda;
  
  cov = quad_form_diag(rho, s);
}

model {
  //generation of local parameters from global parameters
  for (np in 1:NP) local[np,] ~ multi_normal(m,cov);
  
  //model
  for (n in 1:N){
    RT[n] ~ exp_mod_normal(
      base[Par[n]] + rate[Par[n]] * Angle[n] * 
      ((Angle[n]/180)^expo[Par[n]]), 
      sigma[Par[n]], lambda[Par[n]]
      );
  }
  
  //prior
  rho ~ lkj_corr(1);  //uninformative prior
  m_log_expo ~ normal(0,1);  //weakly informative prior
}

generated quantities{
  real m_expo; //mean of expo parameters
  real m_lambda; //mean of lambda parameters
  vector[NP] RT_pred;  //for posterior predictive distribution
  
  m_expo = exp(m_log_expo);
  m_lambda = exp(m_log_lambda);
  for (np in 1:NP) RT_pred[np] = exp_mod_normal_rng(0,sigma[np],lambda[np]);
}
