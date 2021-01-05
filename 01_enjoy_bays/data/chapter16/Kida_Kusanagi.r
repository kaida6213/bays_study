#stan 2.17; rstan 2.17.2; Rtools 3.4; R 3.3; Windows 10で確認
#準備
getwd() #作業ディレクトリを確認してください
library(rstan) #主として使用するrstanパッケージ
library(HDInterval) #最高密度区間によって確信区間を構築するために使用
dat<-read.csv("SDT_data.csv", header = TRUE) 
head(dat) #被験者の反応が記録されているため，確認
#1列目にヒット，2列目に誤警報，3列目にミス，4列目に正棄却

#rstan用にデータの整形
h<-dat[,1] #ヒット
f<-dat[,2] #誤警報
s<-15 #ここでの適切な文の数は15
n<-15 #ここでの不適切な文の数は15
k<-101 #ここでの標本サイズ（n = 101）
dat2 <- list(h=h, f=f, s=s, n=n, k=k) #リスト形式

#stanコード
model <- "
data { //データ記述部分
  int<lower=1> k; //標本サイズ
  int<lower=0> h[k];//個人のヒット
  int<lower=0> f[k];//個人の誤警報
  int<lower=0> s;//シグナルの数
  int<lower=0> n;//ノイズの数
}
parameters { //パラミター記述部分
  vector[k] d; //個人のd
  vector[k] c; //個人のc
  real muc; //cの母平均
  real mud; //dの母平均
  real<lower=0> lambdac; //cの精度（precision）
  real<lower=0> lambdad; //dの精度（precision）
} 
transformed parameters { //変換パラミター記述部分
  real<lower=0,upper=1> thetah[k]; //ヒット率
  real<lower=0,upper=1> thetaf[k]; //誤警報率
  real<lower=0> sigmac; //cの母標準偏差
  real<lower=0> sigmad; //dの母標準偏差
  
  sigmac = inv_sqrt(lambdac); //λをσに変換
  sigmad = inv_sqrt(lambdad);
  
    for(i in 1:k) {
    thetah[i] = Phi(d[i] / 2 - c[i]); //ヒット率は，標準正規分布におけるd/2-cに対応する値
    thetaf[i] = Phi(-d[i] / 2 - c[i]); //誤警報率は，標準正規分布における(-d)/2-cに対応する値
  }
}
model { //モデル記述部分
  muc ~ normal(0, inv_sqrt(.001)); //cの母平均の事前分布
  mud ~ normal(0, inv_sqrt(.001)); //dの母平均の事前分布
  lambdac ~ gamma(.001, .001); //cの精度の事前分布，σに変換される
  lambdad ~ gamma(.001, .001); //dの精度の事前分布，σに変換される
  c ~ normal(muc, sigmac); //個人のcは，集団の平均と標準偏差に従う
  d ~ normal(mud, sigmad); //個人のdは，集団の平均と標準偏差に従う
  h ~ binomial(s, thetah); //個人についての観測であるヒットは，ヒット率とシグナル数を項にもつ二項分布から生成
  f ~ binomial(n, thetaf); //個人についての観測である誤警報は，ヒット率とシグナル数を項にもつ二項分布から生成
}"


#モデルをデータにフィット；rstanの実行
par　<- c("mud", "muc", "sigmad", "sigmac") #モニターするパラミターの設定
inits <- list(list(d=rep(0, 101), c=rep(0, 101), mud=0, muc=0, lambdad=1, lambdac=1)) #初期値の設定
fit <- stan(model_code=model,   #上記モデルを指定
                     data=dat2, #データはdat2
                     pars=par, #パラミターの指定
                     iter=10000, #繰り返し回数，ここでは10000
                     init=inits,#初期値の指定
                     chains=1, #チェイン数の指定
                     thin=1, #間引き区間の設定，ここではなし
                     warmup=1000 #焼却区間の設定，ここでは1000
    )

#パラミターの抽出
mud<-extract(fit)$mud #dの母平均のMCMCサンプル
muc<-extract(fit)$muc #cの母平均のMCMCサンプル
sigmad<-extract(fit)$sigmad #dの母標準偏差のMCMCサンプル
sigmac<-extract(fit)$sigmac #cの母標準偏差のMCMCサンプル
result<-data.frame(mud,muc,sigmad,sigmac) #データフレームにまとめる

#表2
#事後期待値
result_EAPs<-round(apply(result,2,mean),2) #事後期待値を小数点2桁で丸める
result_EAPs
#事後標準偏差
result_SDs<-round(apply(result,2,sd),2)  #事後標準偏差を小数点2桁で丸める
result_SDs
#分位点
result_percentile<-round(t(apply(result,2,quantile,c(.025,.25,.50,.75,.975))),2) #それぞれの分位点を小数点2桁で丸める
result_percentile

#図3
par(mfrow=c(2,2)) #図を四枚のプロットに
plot(density(mud),xlab="",main="(a)") #弁別力の母平均，カーネル密度曲線
polygon(density(mud),col="gray98") #カーネル密度曲線内を塗りつぶし
plot(density(sigmad),xlab="",main="(b)") #弁別力の母標準偏差
polygon(density(sigmad),col="gray98") #カーネル密度曲線内を塗りつぶし
plot(density(muc),xlab="",main="(c)") #バイアスの母平均
polygon(density(muc),col="gray98") #カーネル密度曲線内を塗りつぶし
plot(density(sigmac),xlab="",main="(d)") #カーネル密度曲線内を塗りつぶし
polygon(density(sigmac),col="gray98") #バイアスの母標準偏差

#HDI
result_hdi<-round(hdi(result),2) #最高密度区間による確信区間
result_hdi

#負の弁別力を示す割合
rate<-pnorm(0,mud,sigmad) #MCMCにおける母平均，母標準偏差からの生成量

#表3
rate_EAP<-round(mean(rate),2)#事後期待値
rate_EAP
rate_SD<-round(sd(rate),2)#事後標準偏差
rate_SD
rate_percentile<-round(quantile(rate,c(.025,.25,.5,.75,.975)),2)
rate_percentile

