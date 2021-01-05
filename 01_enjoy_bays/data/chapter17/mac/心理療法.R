#Stan2.17,rstan2.17.3, blavaan 0.3-1, R3.5.0, macOS High Sierra 10.13.3で動作確認済,2018,08,22

rm(list=ls())

# データの読み込みと記述統計量の算出 (表17.1)
library(tidyverse)

dat<-read.csv("datLGM.csv",header=T)[,-1]

sums<-dat %>% summarize_all(funs(mean, sd))

tab1<-matrix(0,2,4)

tab1[1,]<-as.matrix(sums[grep("mean",colnames(sums))])
tab1[2,]<-as.matrix(sums[grep("sd",colnames(sums))])

colnames(tab1)<-c("介入開始前","介入終了後","フォローアップ","共変量")
rownames(tab1)<-c("平均","標準偏差")

tab1

# 記述統計量のプロット (図17.1)

dat.long<-dat %>% gather(., time, value,BaseGRIDHAMD:FuGRIDHAMD)
time<-ifelse(dat.long$time=="BaseGRIDHAMD","0",dat.long$time)
time<-ifelse(time=="PostGRIDHAMD","1",time)
time<-ifelse(time=="FuGRIDHAMD","2",time)

dat.long$time<-time

dat.long %>% mutate(ID=rep(1:20,3))%>% 
  ggplot(aes(x=time,y=value))+ 
  geom_violin(trim=FALSE, fill="gray")+
  geom_boxplot(width=0.1)+
  theme_classic(base_family = "HiraKakuPro-W3")+ 
  scale_x_discrete(labels=c("介入実施前","介入終了後","フォローアップ"))+
  ylab("抑うつ症状 (HAMD)")+xlab(" ")



# 潜在成長曲線モデル: 最尤推定 (表17.2)
library(blavaan)

LGM<-'
i=~1*BaseGRIDHAMD+1*PostGRIDHAMD+1*FuGRIDHAMD
s=~0*BaseGRIDHAMD+1*PostGRIDHAMD+2*FuGRIDHAMD

i~BaseSuppression
s~BaseSuppression

BaseSuppression~~BaseSuppression
BaseSuppression~0*1

'

fit1.growth.ml<-growth(LGM,data=dat) # 最尤法と解こうとすると因子の分散が負値
summary(fit1.growth.ml,ci=T)


# 潜在成長曲線モデル: ベイズ推定 漠然事前分布  (表17.3)

LGM.dp<-'
i=~1*BaseGRIDHAMD+1*PostGRIDHAMD+1*FuGRIDHAMD
s=~0*BaseGRIDHAMD+1*PostGRIDHAMD+2*FuGRIDHAMD

i~BaseSuppression
s~BaseSuppression

i~1
s~1


BaseGRIDHAMD~~BaseGRIDHAMD
PostGRIDHAMD~~PostGRIDHAMD
FuGRIDHAMD~~FuGRIDHAMD

i~~i
s~~s

i~~s

BaseGRIDHAMD~0*1
PostGRIDHAMD~0*1
FuGRIDHAMD~0*1
'


fit1.growth.stan<-blavaan::bgrowth(LGM.dp,data=dat,target="stan", 
                                   n.chains=4,
                                   burnin=1000,
                                   sample=5000,
                                   mcmcfile=T)

summary(fit1.growth.stan)

# 潜在成長曲線モデル: 経験ベイズ事前分布   (表17.3)

LGMddp<-'
i=~1*BaseGRIDHAMD+1*PostGRIDHAMD+1*FuGRIDHAMD
s=~0*BaseGRIDHAMD+1*PostGRIDHAMD+2*FuGRIDHAMD

i~prior("normal(0,21.50)")*1
s~prior("normal(0, 9.02)")*1

i~prior("normal(0,14.92)")*BaseSuppression
s~prior("normal(0,14.92)")*BaseSuppression

BaseGRIDHAMD~~prior("inv_gamma(0.001,0.001)")*BaseGRIDHAMD
PostGRIDHAMD~~prior("inv_gamma(0.001,0.001)")*PostGRIDHAMD
FuGRIDHAMD~~prior("inv_gamma(0.001,0.001)")*FuGRIDHAMD

i~~prior("inv_gamma(0.001,0.001)")*i
s~~prior("inv_gamma(0.001,0.001)")*s

i~~prior("beta(1,1)")*s

BaseGRIDHAMD~0*1
PostGRIDHAMD~0*1
FuGRIDHAMD~0*1


'


fit1.growth.stan.ddp<-blavaan::bsem(LGMddp,data=dat,target="stan",
                                    n.chains=4,
                                    burnin=1000,
                                    sample=5000,
                                    mcmcfile=T)
summary(fit1.growth.stan.ddp)


# 漠然事前分布の推定結果のプロット   (図17.3-17.4)

alpha_I<-data.frame(x=rstan::extract(fit1.growth.stan@external$mcmcout,pars="alpha[1,1,1]")$'alpha[1,1,1]')
alpha_S<-data.frame(x=rstan::extract(fit1.growth.stan@external$mcmcout,pars="alpha[2,1,1]")$'alpha[2,1,1]')

psi_I<-data.frame(x=rstan::extract(fit1.growth.stan@external$mcmcout,pars="psi[1,1,1]")$'psi[1,1,1]')
psi_S<-data.frame(x=rstan::extract(fit1.growth.stan@external$mcmcout,pars="psi[1,1,1]")$'psi[2,2,1]')

beta_I<-data.frame(x=rstan::extract(fit1.growth.stan@external$mcmcout,pars="beta[1,3,1]")$'beta[1,3,1]')
beta_S<-data.frame(x=rstan::extract(fit1.growth.stan@external$mcmcout,pars="beta[2,3,1]")$'beta[2,3,1]')


library(gridExtra)
qIm<-ggplot(alpha_I,aes(x=x))+geom_histogram(alpha=.6,col="white",bins=40,aes(y=..density..), position="identity") + 
  geom_density(aes(x=x,y=..density..),col="black",alpha=.7)+xlab("切片因子の平均")+theme_bw(base_family = "HiraKakuPro-W3")
qSm<-ggplot(alpha_I,aes(x=x))+geom_histogram(alpha=.6,col="white",bins=40,aes(y=..density..), position="identity") + 
  geom_density(aes(x=x,y=..density..),col="black",alpha=.7)+xlab("傾き因子の平均")+theme_bw(base_family = "HiraKakuPro-W3")

qIv<-ggplot(psi_I,aes(x=x))+geom_histogram(alpha=.6,col="white",bins=40,aes(y=..density..), position="identity") + 
  geom_density(aes(x=x,y=..density..),col="black",alpha=.7)+xlab("切片因子の分散")+theme_bw(base_family = "HiraKakuPro-W3")
qSv<-ggplot(psi_I,aes(x=x))+geom_histogram(alpha=.6,col="white",bins=40,aes(y=..density..), position="identity") + 
  geom_density(aes(x=x,y=..density..),col="black",alpha=.7)+xlab("傾き因子の分散")+theme_bw(base_family = "HiraKakuPro-W3")


qIbeta<-ggplot(beta_I,aes(x=x))+geom_histogram(alpha=.6,col="white",bins=40,aes(y=..density..), position="identity") + 
  geom_density(aes(x=x,y=..density..),col="black",alpha=.7)+xlab("共変量から切片因子への回帰係数")+theme_bw(base_family = "HiraKakuPro-W3")
qSbeta<-ggplot(beta_S,aes(x=x))+geom_histogram(alpha=.6,col="white",bins=40,aes(y=..density..), position="identity") + 
  geom_density(aes(x=x,y=..density..),col="black",alpha=.7)+xlab("共変量から傾き因子への回帰係数")+theme_bw(base_family = "HiraKakuPro-W3")


grid.arrange(qIm,qSm,qIv,qSv,ncol=2)
grid.arrange(qIbeta,qSbeta,ncol=2)


# 傾き因子得点と共変量の散布図 (図17.5)

qplot(x=dat$BaseSuppression,y=predict(fit1.growth.stan)[,2])+theme_classic(base_family = "HiraKakuPro-W3")+ylim(2,-9)+
  ylab("抑うつ症状得点の変化(傾き因子得点)")+xlab("感情表出抑制")+geom_point(size=2)


