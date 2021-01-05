#■Stan2.16, rstan 2.17.2, Rtools 3.4, R3.4.2, windows10で動作確認済

getwd()                   #working directoryの確認
library(rstan)            #パッケージrstanの呼び出し
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

(音声<-read.csv("音声.csv", header = TRUE))　　　　　　　　　　　　　　　　　　　　　　#データ”音声”読み込み


#記述統計量
a1<-音声[  1: 17,4];a2<-音声[ 18: 34,4];a3<-音声[ 35: 51,4]; 　　　　 #F0を感情別にa1からa6に代入
a4<-音声[ 52: 68,4];a5<-音声[ 69: 85,4];a6<-音声[ 86: 102,4]      
#a1<-音声[ $:$ ?]の ? を2に変えるとF0レンジ、3だと音圧レベル、4だと発話時間

van<-function(x){mean((x-mean(x))^2)}                     　　　　  #標本分散を計算する関数     

mean(a1);mean(a2);mean(a3);mean(a4);mean(a5);mean(a6);   　　　　   #平均
van(a1);van(a2);van(a3);van(a4);van(a5);van(a6);         　　　　   #分散
sqrt(van(a1));sqrt(van(a2));sqrt(van(a3));               　　　　   #標準偏差
sqrt(van(a4));sqrt(van(a5));sqrt(van(a6));
quantile(a1,type =2);quantile(a2,type =2);quantile(a3,type =2); #％点
quantile(a4,type =2);quantile(a5,type =2);quantile(a6,type =2);


#箱髭図 図１～４
boxplot(音声$F0~音声$emotion, xlab="感情", ylab="F0")
boxplot(音声$F0レンジ~音声$emotion, xlab="感情", ylab="F0レンジ(Hz)")
boxplot(音声$音圧レベル~音声$emotion, xlab="感情", ylab="音圧レベル")
boxplot(音声$発話時間~音声$emotion, xlab="感情", ylab="発話時間(sec)")


#stanスクリプト
RBlockD<-'
//乱塊計画・被検者内1要因計画
data { 
  int<lower=0>  n;                     //全データ数 
  int<lower=0>  J;                     //群数
  int<lower=0>  K;                     //ブロック数
  vector[n]     y;                     //基準変数   
  int<lower=0>  j[n];                  //分類変数A 
  int<lower=0>  k[n];                  //ブロック変数B 
}
parameters {
  vector[J]   a;                       //各群の効果
  vector[K]   b;                       //ブロックの効果
  real<lower=0> sigma;                 //誤差SD
  real<lower=0> s_b;                   //ブロックSD
}
transformed parameters {
}
model {
    y ~ normal(a[j]+b[k], sigma);
    b ~ normal(0, s_b);
}
generated quantities{
  real mu;					//全平均
  real s_a;					//要因SD
  mu = mean(a);
  s_a =sqrt(variance(a)*(J-1)/J);
}
';


par<-c("a","mu","sigma","s_a","s_b","b")
fitran<-stan(model_code =RBlockD,                       
  data=list(n=length(音声$F0),J=max(音声$感情),
    K=max(音声$発声者),y=音声$F0,j=音声$感情,
    k=音声$発声者),                               #入力データ
  pars=par, seed=1234, chains=5, warmup=1000,  iter=21000)
prob<-c(0.025, 0.5, 0.975) #確率点の定義
print(fitran,digits_summary=3, probs =prob)
#音声$F0のF0をF0レンジ、音圧レベル、発話時間に変更することでそれぞれの分析が可能


#説明率・効果量・研究仮説が正しい確率
extran<-extract(fitran, par)

#差がある確率
mean(extran$a[,1]>extran$a[,2])
mean(extran$a[,1]>extran$a[,3])
mean(extran$a[,1]>extran$a[,4])
mean(extran$a[,1]>extran$a[,5])
mean(extran$a[,1]>extran$a[,6])
mean(extran$a[,2]>extran$a[,3])
mean(extran$a[,2]>extran$a[,4])
mean(extran$a[,2]>extran$a[,5])
mean(extran$a[,2]>extran$a[,6])
mean(extran$a[,3]>extran$a[,4])
mean(extran$a[,3]>extran$a[,5])
mean(extran$a[,3]>extran$a[,6])
mean(extran$a[,4]>extran$a[,5])
mean(extran$a[,4]>extran$a[,6])
mean(extran$a[,5]>extran$a[,6])

#説明率・効果量
eta2 = (extran$s_a^2)/((extran$s_a^2)+(extran$sigma^2));
delta = extran$s_a/extran$sigma;
mean(eta2);sd(eta2);quantile(eta2,probs=prob)
mean(delta);sd(delta);quantile(delta,probs=prob)

