#Stan2.17,rstan2.17.3,Rtools3.4,R3.4.3,windows10で動作確認済,2018,08,17
getwd()       
source('myfunc.R')      #自作関数の読み込み
library(rstan)
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())
x01<-read.csv("楓.csv", header = TRUE)

#データクリーニング
x<-x01[x01$AV<44,]; #AVの上限は理論的に43
xm<-x[x$sex==1,]
xf<-x[x$sex==0,]
dig=1
(jsex<-ifelse(x$sex==1,"男性","女性"))
(jlover<-ifelse(x$lover==1,"いる","いない"))

#ヒストグラムと箱髭図　図１
max(x$AV);min(x$AV)
par(mfrow=c(1,2))
hist(x$AV,col=8,main="",xlab="",ylab="",cex.lab=2.0,cex.axis=2.0)
boxplot(x$AV~jlover+jsex,xlab="",ylab="",cex.lab=1.0,cex.axis=2.0)
abline(h=0, lwd=2.0)
par(mfrow=c(1,1))
#dev.copy2eps(file="z01.eps",family='Japan1')


#平均値の計算　表１，２
nrow(x)
sort(x$AV)
round(mean(x$AV),dig)
round(tapply(x$AV,x$sex,mean),dig)
round(tapply(x$AV,x$lover,mean),dig)
round(tapply(xm$AV,xm$lover,mean),dig)
round(tapply(xf$AV,xf$lover,mean),dig)

#stanスクリプト
AR01<-'
//Aggregated Response法
functions{
  vector zero_sum_vector(int a, vector m1A){//主効果ゼロ和
    vector[a] muA;            
    for(i in 1:(a-1)){ muA[i]  = m1A[i];}
    muA[a]  = -sum(m1A);
    return(muA);
  }
  matrix zero_sum_matrix(int a, int b, matrix m1AB){//交互作用ゼロ和
    vector [a-1] m1a;   
    vector [b-1] m1b;   
    matrix [a,b] muAB;  
    for(i in 1:(a-1)){ m1a[i]  = 0.0;}
    for(j in 1:(b-1)){ m1b[j]  = 0.0;}
    for(i in 1:(a-1)){ for(j in 1:(b-1)){muAB[i,j] =m1AB[i,j];}}
    for(i in 1:(a-1)){ for(j in 1:(b-1)){m1a[i] =m1a[i]+m1AB[i,j];}}
    for(j in 1:(b-1)){ for(i in 1:(a-1)){m1b[j] =m1b[j]+m1AB[i,j];}}
    for(i in 1:(a-1)){muAB[i,b] = (-1)*m1a[i]; }
    for(j in 1:(b-1)){muAB[a,j] = (-1)*m1b[j]; }
    muAB[a,b] = sum(m1a);     //ここはマイナスではない
    return(muAB);
  }
}
data {
  int<lower=0> n;                      //データ数
  int<lower=0> m;                      //マスク数
  int          x[n];                   //データ
  int<lower=0>  A[n];                  //分類変数A 
  int<lower=0>  B[n];                  //分類変数B 
  int          mask[m];                //マスク要素
}
transformed data{
  int<lower=0> a;                      //A水準数
  int<lower=0> b;                      //B水準数
  a=2;
  b=2;
}
parameters {                           //母数定義部
  real<lower=0> mu;                    //全平均
  vector          [a-1] m1A;           //A平均1つ少ない
  vector          [b-1] m1B;           //B平均1つ少ない
  matrix      [a-1,b-1] m1AB;          //交互作用1つ少ない
  real<lower=0>       sigma_e;         //誤差標準偏差
}
transformed parameters {               //母数変換部
  vector                   [a] muA;    //A平均
  vector                   [b] muB;    //B平均
  matrix                 [a,b] muAB;   //交互作用
  muA =zero_sum_vector(a, m1A);
  muB =zero_sum_vector(b, m1B);
  muAB=zero_sum_matrix(a, b, m1AB);
}
model {                                //モデル記述部
  real        t[n];                    //各データの平均
  real         j;                      //可能性のあるmaskの数 1
  int         jj;                      //可能性のあるmaskの数 2
  real         pr;                     //可能性のあるmaskの数の逆数
  vector [m]  lp;
  for(i in 1:n){
    t[i]=mu+muA[A[i]]+muB[B[i]]+muAB[A[i],B[i]];
    j = 0.0;
    for(k in 1:m) if (0 <= x[i]-mask[k]){ j =j +1.0;}
    pr = 1.0/j;
    jj=0;
    for(k in 1:m){                     //kはマスク
      if (0 <= x[i]-mask[k]) {         //可能性のあるmask取り出し
        jj=jj+1;
        lp[jj] = log(pr)+normal_lpdf( x[i]-mask[k] | t[i], sigma_e);
      }
    }
    target += log_sum_exp( lp[1:jj] );
  }
}
generated quantities{                  //生成量
  real sigma_a;            //要因AのSD
  real sigma_b;            //要因BのSD
  real sigma_ab;           //交互作用ABのSD
  sigma_a =sqrt(variance(muA)*(a-1)/a);
  sigma_b =sqrt(variance(muB)*(b-1)/b);
  sigma_ab=sqrt(variance(muAB)*((a*b)-1)/(a*b));
}
';

#stan実行部
# 本文は「chains=20,warmup=1000,iter=51000」で実行しています。
mask<-c(-12:1,1:12); m<-length(mask)
par<-c("mu","muA","muB","muAB","sigma_a","sigma_b","sigma_ab","sigma_e") 
fitAR<-stan(model_code =AR01,
  data=list(n=length(x$AV),m=m,x=x$AV,A=x$sex+1,B=x$lover+1,mask=mask),
  pars=par,seed=1234,chains=5,warmup=1000,iter=21000,init=mean(x))
prob<-c(0.025,0.05, 0.5, 0.975) #確率点の定義
print(fitAR,digits_summary=3, probs =prob)
#表３
round(summary(fitAR)$summary[,c("mean","sd","2.5%","50%","97.5%")],2)

#事後分布の取り出し・MAP推定値
extAR<-extract(fitAR, par)
MAP(extAR$mu,2)
MAP(extAR$muA[,2],2)
MAP(extAR$muB[,2],2)
MAP(extAR$muAB[,2,2],2)
MAP(extAR$sigma_e,2)

#男性の方が女性より視聴日数が多いPHC
mean(extAR$muA[,2]>0)

#事後分布のヒストグラム　図２
par(mfrow=c(1,2))
hist(extAR$muA[,2],breaks=100,main="",xlab="要因性別・水準男性の効果",ylab="",cex.lab=2.0,cex.axis=2.0)
hist(extAR$muB[,2],breaks=100,main="",xlab="要因恋人・水準いるの効果",ylab="",cex.lab=2.0,cex.axis=2.0)
par(mfrow=c(1,1))
#dev.copy2eps(file="z02.eps",family='Japan1')

#女性の月平均鑑賞日数の事後分布の要約統計量
gen_quan02(extAR$mu+extAR$muA[,1],2)

#生成量・説明率　表４
sigma_T<-extAR$sigma_a^2 +extAR$sigma_b^2 +extAR$sigma_ab^2 +extAR$sigma_e^2 
gen_quan01(extAR$sigma_a^2 /sigma_T,2)
gen_quan01(extAR$sigma_b^2 /sigma_T,2)
gen_quan01(extAR$sigma_ab^2 /sigma_T,2)

#男性のほうが女性より平均鑑賞日数が$c$日多いというPHC確率曲線　図３
phc01(seq01=seq(0,12,0.5),a1=extAR$muA[,2],a2=extAR$muA[,1],xlab="",2.0)
#dev.copy2eps(file="z03.eps",family='Japan1')

#女性の月あたり平均鑑賞日数は$c$日より多いというPHC確率曲線　図４
phc01(seq01=seq(4,10,0.25),a1=extAR$mu+extAR$muA[,1],a2=0,xlab="",2.0)
#dev.copy2eps(file="z04.eps",family='Japan1')

#生成量・セル平均
cell<-matrix(0,nrow(extAR$mu),4)
cell[,1]=extAR$mu+extAR$muA[,1]+extAR$muB[,1]+extAR$muAB[,1,1]
cell[,2]=extAR$mu+extAR$muA[,1]+extAR$muB[,2]+extAR$muAB[,1,2]
cell[,3]=extAR$mu+extAR$muA[,2]+extAR$muB[,1]+extAR$muAB[,2,1]
cell[,4]=extAR$mu+extAR$muA[,2]+extAR$muB[,2]+extAR$muAB[,2,2]

#表５
gen_quan01(cell[,1],2)
gen_quan01(cell[,2],2)
gen_quan01(cell[,3],2)
gen_quan01(cell[,4],2)

gen_quan02(cell[,1],2)
gen_quan02(cell[,2],2)
gen_quan02(cell[,3],2)
gen_quan02(cell[,4],2)

#表６
phc02(cell[,c(1:4)],c=0, digits=3)


