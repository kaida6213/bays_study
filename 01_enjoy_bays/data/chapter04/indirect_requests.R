#rstan 2.17.3, R3.3.3, windows10で動作確認済(2018.08.21)
install.packages("ggplot2")
install.packages("psych")

library(rstan)
library(ggplot2)
library(psych)


# データの読み込み  ------------------------------------------------------------------
d<-read.csv("data_hirakawa.csv")
#sex 1=男性, 2=女性
#p1~p7 7つの場面それぞれへの反応　1=要求の解釈が正しいと判断, 0=要求の解釈が間違っていると判断
#y 要求の解釈が正しいと判断した数



# 解釈率が個人ごとに異なると仮定した2項分布による分析 ----------------------------------------------------------------
# MCMCサンプリング(本文ではiter=30000, warmup=5000で実行している)
model<-stan_model('binom_normal.stan')
data<-list(N=nrow(d), y=d$y)
fit_binom_normal <- sampling(model,data,iter=10000,warmup=5000,chains=4,cores=4, seed="1234")

# 収束診断
fit<-fit_binom_normal
summary(fit)$summary[,"Rhat"]
## すべてのパラメーターのRhatが1.01以下か判定
all(summary(fit)$summary[,"Rhat"]<=1.01, na.rm=T)


# パラメーターmu, sigmaの事後分布(図1)
mu<-rstan::extract(fit)$mu
sigma<-rstan::extract(fit)$sigma
res<-data.frame(sample=c(mu, sigma),pars=rep(c("mu","sigma"), each=length(mu)))

ggplot(res,aes(x=sample,y=..density..))+
  geom_histogram(position="identity")+
  geom_density(position="identity", size=1.2)+
  facet_wrap(~pars, scales="free")+
  labs(y = "確率密度")+
  theme_bw(base_size = 20)



# パラメーターmu, sigmaの要約統計量(表2)
parameters=c("mu","sigma")
round(summary(fit)$summary[parameters,c("mean","sd","2.5%","50%","97.5%")],3)

##MAP推定量
MAP<-function(ext,dig=3){round(density(ext)$x[which.max(density(ext)$y)],dig)}　#MAP推定値を与える関数
ext<-extract(fit, parameters)
MAP(ext$mu,3)
MAP(ext$sigma,3)


#thetaの変動(図2)
theta<-rstan::extract(fit)$theta
q_95<-quantile(theta, probs=c(0.025,0.975))　#2.5%, 97.5%タイル点
q_80<-quantile(theta, probs=c(0.10,0.90))  #10%, 90%タイル点

dens <- density(theta)
dat<-data.frame(X=dens$x, Y=dens$y)

ggplot(dat,aes(x=X,y=Y))+
  geom_line(data=dat, aes(x=X, y=Y))+
  geom_ribbon(data=subset(dat, X>=q_95[1] & X<=q_95[2]), aes(x=X, ymin=0, ymax=Y),fill = "grey60")+
  geom_ribbon(data=subset(dat, X>=q_80[1] & X<=q_80[2]), aes(x=X, ymin=0, ymax=Y),fill = "grey40")+
  labs(y = "確率密度")+
  theme_bw(base_size = 30)


# パラメーターthetaの要約統計量
parameters="theta"
round(summary(fit)$summary[parameters,c("mean","sd","2.5%","50%","97.5%")],3)　
round(density(theta)$x[which.max(density(theta)$y)],3)



# 要求の解釈率が異なるグループの存在を仮定した混合2項分布による分析 ----------------------------------------------------------------
# MCMCサンプリング(本文ではiter=30000, warmup=5000で実行している)
model<-stan_model('binom_mixture.stan')
data<-list(N=nrow(d), y=d$y)
fit_binom_mixture <- sampling(model,data,iter=10000,warmup=5000,chains=4,cores=4, seed="1234")

# 収束診断
fit<-fit_binom_mixture
summary(fit)$summary[,"Rhat"]
## すべてのパラメーターのRhatが1.01以下か判定
all(summary(fit)$summary[,"Rhat"]<=1.01, na.rm=T)

# パラメーターpi, thetaの要約統計量(表3)
parameters=c("pi","theta[1]","theta[2]")
round(summary(fit)$summary[parameters,c("mean","sd","2.5%","50%","97.5%")],3)

##MAP推定量
MAP<-function(ext,dig=3){round(density(ext)$x[which.max(density(ext)$y)],dig)} #MAP推定値を与える関数
ext<-extract(fit, parameters)
MAP(ext$pi,3)
MAP(ext$`theta[1]`,3)
MAP(ext$`theta[2]`,3)


# パラメーターpi, thetaの事後分布（図3）
pi<-rstan::extract(fit)$pi
theta_1<-rstan::extract(fit)$theta[,1]
theta_2<-rstan::extract(fit)$theta[,2]

res<-data.frame(sample=c(pi, theta_1, theta_2),pars=rep(c("pi","theta_1","theta_2"), each=length(pi)))

ggplot(res,aes(x=sample,y=..density..))+
  geom_histogram(position="identity")+
  geom_density(position="identity", size=1.2)+
  facet_wrap(~pars, scales="free")+
  labs(y = "確率密度")+
  theme_bw(base_size = 15)
