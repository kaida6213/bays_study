#rstan 2.17.3, R3.3.3, windows10�œ���m�F��(2018.08.21)
install.packages("ggplot2")
install.packages("psych")

library(rstan)
library(ggplot2)
library(psych)


# �f�[�^�̓ǂݍ���  ------------------------------------------------------------------
d<-read.csv("data_hirakawa.csv")
#sex 1=�j��, 2=����
#p1~p7 7�̏�ʂ��ꂼ��ւ̔����@1=�v���̉��߂��������Ɣ��f, 0=�v���̉��߂��Ԉ���Ă���Ɣ��f
#y �v���̉��߂��������Ɣ��f������



# ���ߗ����l���ƂɈقȂ�Ɖ��肵��2�����z�ɂ�镪�� ----------------------------------------------------------------
# MCMC�T���v�����O(�{���ł�iter=30000, warmup=5000�Ŏ��s���Ă���)
model<-stan_model('binom_normal.stan')
data<-list(N=nrow(d), y=d$y)
fit_binom_normal <- sampling(model,data,iter=10000,warmup=5000,chains=4,cores=4, seed="1234")

# �����f�f
fit<-fit_binom_normal
summary(fit)$summary[,"Rhat"]
## ���ׂẴp�����[�^�[��Rhat��1.01�ȉ�������
all(summary(fit)$summary[,"Rhat"]<=1.01, na.rm=T)


# �p�����[�^�[mu, sigma�̎��㕪�z(�}1)
mu<-rstan::extract(fit)$mu
sigma<-rstan::extract(fit)$sigma
res<-data.frame(sample=c(mu, sigma),pars=rep(c("mu","sigma"), each=length(mu)))

ggplot(res,aes(x=sample,y=..density..))+
  geom_histogram(position="identity")+
  geom_density(position="identity", size=1.2)+
  facet_wrap(~pars, scales="free")+
  labs(y = "�m�����x")+
  theme_bw(base_size = 20)



# �p�����[�^�[mu, sigma�̗v�񓝌v��(�\2)
parameters=c("mu","sigma")
round(summary(fit)$summary[parameters,c("mean","sd","2.5%","50%","97.5%")],3)

##MAP�����
MAP<-function(ext,dig=3){round(density(ext)$x[which.max(density(ext)$y)],dig)}�@#MAP����l��^����֐�
ext<-extract(fit, parameters)
MAP(ext$mu,3)
MAP(ext$sigma,3)


#theta�̕ϓ�(�}2)
theta<-rstan::extract(fit)$theta
q_95<-quantile(theta, probs=c(0.025,0.975))�@#2.5%, 97.5%�^�C���_
q_80<-quantile(theta, probs=c(0.10,0.90))  #10%, 90%�^�C���_

dens <- density(theta)
dat<-data.frame(X=dens$x, Y=dens$y)

ggplot(dat,aes(x=X,y=Y))+
  geom_line(data=dat, aes(x=X, y=Y))+
  geom_ribbon(data=subset(dat, X>=q_95[1] & X<=q_95[2]), aes(x=X, ymin=0, ymax=Y),fill = "grey60")+
  geom_ribbon(data=subset(dat, X>=q_80[1] & X<=q_80[2]), aes(x=X, ymin=0, ymax=Y),fill = "grey40")+
  labs(y = "�m�����x")+
  theme_bw(base_size = 30)


# �p�����[�^�[theta�̗v�񓝌v��
parameters="theta"
round(summary(fit)$summary[parameters,c("mean","sd","2.5%","50%","97.5%")],3)�@
round(density(theta)$x[which.max(density(theta)$y)],3)



# �v���̉��ߗ����قȂ�O���[�v�̑��݂����肵������2�����z�ɂ�镪�� ----------------------------------------------------------------
# MCMC�T���v�����O(�{���ł�iter=30000, warmup=5000�Ŏ��s���Ă���)
model<-stan_model('binom_mixture.stan')
data<-list(N=nrow(d), y=d$y)
fit_binom_mixture <- sampling(model,data,iter=10000,warmup=5000,chains=4,cores=4, seed="1234")

# �����f�f
fit<-fit_binom_mixture
summary(fit)$summary[,"Rhat"]
## ���ׂẴp�����[�^�[��Rhat��1.01�ȉ�������
all(summary(fit)$summary[,"Rhat"]<=1.01, na.rm=T)

# �p�����[�^�[pi, theta�̗v�񓝌v��(�\3)
parameters=c("pi","theta[1]","theta[2]")
round(summary(fit)$summary[parameters,c("mean","sd","2.5%","50%","97.5%")],3)

##MAP�����
MAP<-function(ext,dig=3){round(density(ext)$x[which.max(density(ext)$y)],dig)} #MAP����l��^����֐�
ext<-extract(fit, parameters)
MAP(ext$pi,3)
MAP(ext$`theta[1]`,3)
MAP(ext$`theta[2]`,3)


# �p�����[�^�[pi, theta�̎��㕪�z�i�}3�j
pi<-rstan::extract(fit)$pi
theta_1<-rstan::extract(fit)$theta[,1]
theta_2<-rstan::extract(fit)$theta[,2]

res<-data.frame(sample=c(pi, theta_1, theta_2),pars=rep(c("pi","theta_1","theta_2"), each=length(pi)))

ggplot(res,aes(x=sample,y=..density..))+
  geom_histogram(position="identity")+
  geom_density(position="identity", size=1.2)+
  facet_wrap(~pars, scales="free")+
  labs(y = "�m�����x")+
  theme_bw(base_size = 15)