library(psych)
library(rstan)
x<-read.csv("Blood.csv", header = TRUE)

#LqvΚ
describe(x[3:9])
describeBy(x[3:9], group = x$Bld_types)

#stanXNvg
one_factor_normal <-'
data { 
int<lower=0> n;                              //f[^ 
int<lower=0> a;                              //A   
vector[n]    y;                              //Α«l   
int<lower=0> A[n];                           //A   
real mL;real mH;real sL;real sH;             //Oͺz
}
parameters {
vector<lower=mL,upper=mH>[a] muA;            //A½Ο
real<lower=sL,upper=sH>   sigmaE;            //λ·SD
}
model {
for(i in 1:n){y[i]~normal(muA[A[i]],sigmaE);}//³Kͺz
}
generated quantities{
real<lower=sL,upper=sH>  sigmaA;             //vφASD
real<lower=0,upper=1>      eta2;             //ΰΎ¦
real                         mu;             //S½Ο
vector                   [a] aj;             //AψΚ
sigmaA  = sqrt(variance(muA)*(a-1)/a);       
eta2  = pow(sigmaA,2)/(pow(sigmaA,2)+pow(sigmaE,2));
mu  = mean(muA);
for (i in 1:a){aj[i]  = muA[i]-mu;}
}
';

#stanΐs
par<-c("muA","sigmaE","sigmaA","eta2","mu","aj")
fitnorm_P<-stan(model_code = one_factor_normal,
                data=list(n=1427, a=4, A=x$Bld_code,
                          y=x$P, mL=0, mH=100, sL=0, sH=100),
                pars=par, seed=1234, chains=5, warmup=1000, iter=21000)

#stanΐsΚ
print(fitnorm_P,digits_summary=3)

#γͺzΜ²«o΅
extnorm<-extract(fitnorm_P, par)

#½ΟlΜ·FΦ
E1betw_level<-function(x,degits=3,I,J,cr1=F)
{
  G<-matrix(0,length(x$mu),5)
  G[,1] <- x$muA[,I]-x$muA[,J];
  G[,2] <- G[,1]/x$sigmaE;
  G[,3] <- pnorm(x$muA[,I],x$muA[,J],x$sigmaE);
  G[,4] <- pnorm((G[,2]/sqrt(2)), 0.0, 1.0);
  lab<-c("½ΟlΜ·","ψΚΚ","ρd‘x","Dz¦")
  co<-4
  if(is.numeric(cr1)){  co<-co+1;
  G[,5] <- pnorm(((G[,1]-cr1)/(sqrt(2)*x$sigmaE)), 0.0, 1.0);
  lab<-c(lab,paste("θγ¦(",cr1,")",sep=""));}
  Gc<-cbind(
    apply(G[,1:co],2,mean),
    apply(G[,1:co],2,sd),
    t(apply(G[,1:co],2,quantile, probs=c(0.025, 0.05, 0.5, 0.95, 0.975)))
  )
  colnames(Gc)<-c("EAP","post.sd","2.5%","5%","50%","95%","97.5%")
  rownames(Gc)<-lab
  round(Gc,degits)
}

#A^ΖB^Μδr
E1betw_level(extnorm,degits=3,3,2,cr1=F)

#A^ΖO^Μδr
E1betw_level(extnorm,degits=3,3,1,cr1=F)
