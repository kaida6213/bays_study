library(psych)
library(rstan)
x<-read.csv("Blood.csv", header = TRUE)

#記述統計量
describe(x[3:9])
describeBy(x[3:9], group = x$Bld_types)

#stanスクリプト
one_factor_normal <-'
data { 
int<lower=0> n;                              //データ数 
int<lower=0> a;                              //A水準数   
vector[n]    y;                              //特性値   
int<lower=0> A[n];                           //A水準   
real mL;real mH;real sL;real sH;             //事前分布
}
parameters {
vector<lower=mL,upper=mH>[a] muA;            //A平均
real<lower=sL,upper=sH>   sigmaE;            //誤差SD
}
model {
for(i in 1:n){y[i]~normal(muA[A[i]],sigmaE);}//正規分布
}
generated quantities{
real<lower=sL,upper=sH>  sigmaA;             //要因ASD
real<lower=0,upper=1>      eta2;             //説明率
real                         mu;             //全平均
vector                   [a] aj;             //A効果
sigmaA  = sqrt(variance(muA)*(a-1)/a);       
eta2  = pow(sigmaA,2)/(pow(sigmaA,2)+pow(sigmaE,2));
mu  = mean(muA);
for (i in 1:a){aj[i]  = muA[i]-mu;}
}
';

#stan実行部
par<-c("muA","sigmaE","sigmaA","eta2","mu","aj")
fitnorm_P<-stan(model_code = one_factor_normal,
                data=list(n=1427, a=4, A=x$Bld_code,
                          y=x$P, mL=0, mH=100, sL=0, sH=100),
                pars=par, seed=1234, chains=5, warmup=1000, iter=21000)

#stan実行結果
print(fitnorm_P,digits_summary=3)

#事後分布の抜き出し
extnorm<-extract(fitnorm_P, par)

#平均値の差：関数
E1betw_level<-function(x,degits=3,I,J,cr1=F)
{
  G<-matrix(0,length(x$mu),5)
  G[,1] <- x$muA[,I]-x$muA[,J];
  G[,2] <- G[,1]/x$sigmaE;
  G[,3] <- pnorm(x$muA[,I],x$muA[,J],x$sigmaE);
  G[,4] <- pnorm((G[,2]/sqrt(2)), 0.0, 1.0);
  lab<-c("平均値の差","効果量","非重複度","優越率")
  co<-4
  if(is.numeric(cr1)){  co<-co+1;
  G[,5] <- pnorm(((G[,1]-cr1)/(sqrt(2)*x$sigmaE)), 0.0, 1.0);
  lab<-c(lab,paste("閾上率(",cr1,")",sep=""));}
  Gc<-cbind(
    apply(G[,1:co],2,mean),
    apply(G[,1:co],2,sd),
    t(apply(G[,1:co],2,quantile, probs=c(0.025, 0.05, 0.5, 0.95, 0.975)))
  )
  colnames(Gc)<-c("EAP","post.sd","2.5%","5%","50%","95%","97.5%")
  rownames(Gc)<-lab
  round(Gc,degits)
}

#A型とB型の比較
E1betw_level(extnorm,degits=3,3,2,cr1=F)

#A型とO型の比較
E1betw_level(extnorm,degits=3,3,1,cr1=F)
