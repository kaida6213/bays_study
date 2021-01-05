
rm(list=ls())
wd <- getwd()
setwd(wd)

################################################################################
## for the first time
# url <- "http://openpsychometrics.org/_rawdata/NPI.zip"
# filename <- "NPI.zip"
# download.file(url,filename,mode = "wb")
# unzip("NPI.zip")

# install.packages("tidyverse")
# install.packages("rstan")
# install.packages("psych")
# install.packages("grid")
################################################################################

library(tidyverse) ## Data Handling
library(rstan) ## Bayesian estimation
library(psych) ## logistic 関数使用のため
library(grid) ## plotの分割表示
options(max.print = 999999) ## print 表示件数の変更
options(mc.cores = parallel::detectCores()) ## PCの最大core数を使ってRstanを使用
rstan_options(auto_write = TRUE) ## コンパイルしたモデルを(上書き)保存

dat <- read.csv("NPI/data.csv") %>%    ## 読み込み
  filter(gender <= 2 & gender > 0) %>% ## 男性と女性のデータのみを抽出
  filter(age == 20)                    ## 年齢が20歳のデータのみを抽出

y <- data.frame(dat[,2:41])            ## 2列目から41列目のデータを使用
y[y==0] <- NA                          ## y=0のデータを欠損値とする
y <- data.frame(y,dat$gender)          ## 項目回答行列+性別列データフレームを作成
y <- y-1                               ## 全ての変数から1を引く(0,1として扱うため)
y <- na.omit(y)                        ## 欠損値除去

## 逆転項目の処理
y[c("Q4","Q5","Q7","Q9","Q10","Q15","Q17","Q18","Q19","Q20","Q22","Q23","Q26","Q28","Q32","Q35","Q40")] <- (1)- y[c("Q4","Q5","Q7","Q9","Q10","Q15","Q17","Q18","Q19","Q20","Q22","Q23","Q26","Q28","Q32","Q35","Q40")]

## もしデータをサンプリングして使いたい場合,以下をコメント外して使う
# I <- 100 #sampling
# set.seed(1234)
# y <- sample_n(y, size = I)

y.gender <- y$dat.gender ## 新たな性別変数オブジェクト作成
y <- y[,-41] ## 性別変数の除去
y %>% summarise_each(dplyr::funs(sum)) %>% data.frame() ## 集計


I <- ncol(y) ## Items (評価する項目数)
J <- nrow(y) ## Respondents (回答者数)
## Stanでベクトル化して回すための添え字を作成
## Stanでベクトル化すると推定が早くなります
j <- rep(1:J, times = I) ## 1-20を200回反復するベクトルを作成(1,2,3,...,18,19,20)
i <- rep(1:I, each = J) ## 1-200を各20回ずつ表示させるベクトルを作成(1,1,1,...,200,200,200)
g <- rep(y.gender, each =I) ## 性別識別子

y <- as.vector(as.matrix(y)) ## 回答者*項目のデータフレームをベクトル化
N <- length(y) ## Number of trials (respondents * items) # yをベクトル化したので総試行数（回答者数*項目数）を算出

ip.dat <- list(N = N,
               J = J,
               I = I,
               g = g,
               j = j,
               ai = i, ## 小文字のiが､著者のStanでうまく認識されなかったのでaiに変更
               y = y)

ip.model <- stan_model("model/IR2PL.NotGender.BF.mac.stan") ## 性別効果なしモデルの読み込み(コンパイル)
ip.res <- sampling(ip.model,ip.dat,
                   init='random',
                   iter=5500,warmup=500,chain=4,seed=1234,thin=10)
gc(t,t);gc(t,t) ## キャッシュをクリア(動作が重くならないようにしている)

stan_rhat(ip.res) ## Rhatの確認
stan_trace(ip.res) ## traceplotの確認
stan_ess(ip.res) ## ess(effective sample size)の確認
stan_ac(ip.res) ## auto correlationの確認
stan_dens(ip.res) ## 事後分布の確認

################################################################################
##### 
##### figure 03
##### 
################################################################################

A.alpha <- get_posterior_mean(ip.res,"alpha")[,5]
A.beta <- get_posterior_mean(ip.res,"beta")[,5]
A.albe.pm <- data.frame(A.alpha,A.beta)
A.albe.pm <- data.frame(t(A.albe.pm))
head(A.albe.pm)
colnames(A.albe.pm) <- paste("Q",1:40,sep = "")
# formattable::formattable(round(A.albe.pm[,1:20],2))

A.theta <- seq(-4,4,length.out=201)

A.prob <- vector("list",40)
A.prob[[1]] <- logistic(A.alpha[1]*(A.theta-A.beta[1]))
A.prob[[2]] <- logistic(A.alpha[2]*(A.theta-A.beta[2]))
A.prob[[3]] <- logistic(A.alpha[3]*(A.theta-A.beta[3]))
A.prob[[4]] <- logistic(A.alpha[4]*(A.theta-A.beta[4]))
A.prob[[5]] <- logistic(A.alpha[5]*(A.theta-A.beta[5]))
A.prob[[6]] <- logistic(A.alpha[6]*(A.theta-A.beta[6]))
A.prob[[7]] <- logistic(A.alpha[7]*(A.theta-A.beta[7]))
A.prob[[8]] <- logistic(A.alpha[8]*(A.theta-A.beta[8]))
A.prob[[9]] <- logistic(A.alpha[9]*(A.theta-A.beta[9]))
A.prob[[10]] <- logistic(A.alpha[10]*(A.theta-A.beta[10]))
A.prob[[11]] <- logistic(A.alpha[11]*(A.theta-A.beta[11]))
A.prob[[12]] <- logistic(A.alpha[12]*(A.theta-A.beta[12]))
A.prob[[13]] <- logistic(A.alpha[13]*(A.theta-A.beta[13]))
A.prob[[14]] <- logistic(A.alpha[14]*(A.theta-A.beta[14]))
A.prob[[15]] <- logistic(A.alpha[15]*(A.theta-A.beta[15]))
A.prob[[16]] <- logistic(A.alpha[16]*(A.theta-A.beta[16]))
A.prob[[17]] <- logistic(A.alpha[17]*(A.theta-A.beta[17]))
A.prob[[18]] <- logistic(A.alpha[18]*(A.theta-A.beta[18]))
A.prob[[19]] <- logistic(A.alpha[19]*(A.theta-A.beta[19]))
A.prob[[20]] <- logistic(A.alpha[20]*(A.theta-A.beta[20]))
A.prob[[21]] <- logistic(A.alpha[21]*(A.theta-A.beta[21]))
A.prob[[22]] <- logistic(A.alpha[22]*(A.theta-A.beta[22]))
A.prob[[23]] <- logistic(A.alpha[23]*(A.theta-A.beta[23]))
A.prob[[24]] <- logistic(A.alpha[24]*(A.theta-A.beta[24]))
A.prob[[25]] <- logistic(A.alpha[25]*(A.theta-A.beta[25]))
A.prob[[26]] <- logistic(A.alpha[26]*(A.theta-A.beta[26]))
A.prob[[27]] <- logistic(A.alpha[27]*(A.theta-A.beta[27]))
A.prob[[28]] <- logistic(A.alpha[28]*(A.theta-A.beta[28]))
A.prob[[29]] <- logistic(A.alpha[29]*(A.theta-A.beta[29]))
A.prob[[30]] <- logistic(A.alpha[30]*(A.theta-A.beta[30]))
A.prob[[31]] <- logistic(A.alpha[31]*(A.theta-A.beta[31]))
A.prob[[32]] <- logistic(A.alpha[32]*(A.theta-A.beta[32]))
A.prob[[33]] <- logistic(A.alpha[33]*(A.theta-A.beta[33]))
A.prob[[34]] <- logistic(A.alpha[34]*(A.theta-A.beta[34]))
A.prob[[35]] <- logistic(A.alpha[35]*(A.theta-A.beta[35]))
A.prob[[36]] <- logistic(A.alpha[36]*(A.theta-A.beta[36]))
A.prob[[37]] <- logistic(A.alpha[37]*(A.theta-A.beta[37]))
A.prob[[38]] <- logistic(A.alpha[38]*(A.theta-A.beta[38]))
A.prob[[39]] <- logistic(A.alpha[39]*(A.theta-A.beta[39]))
A.prob[[40]] <- logistic(A.alpha[40]*(A.theta-A.beta[40]))

ggdf <- data.frame(A.theta,
                   A.prob[[1]],A.prob[[2]],A.prob[[3]],A.prob[[4]],A.prob[[5]],
                   A.prob[[6]],A.prob[[7]],A.prob[[8]],A.prob[[9]],A.prob[[10]],
                   A.prob[[11]],A.prob[[12]],A.prob[[13]],A.prob[[14]],A.prob[[15]],
                   A.prob[[16]],A.prob[[17]],A.prob[[18]],A.prob[[19]],A.prob[[20]],
                   A.prob[[21]],A.prob[[22]],A.prob[[23]],A.prob[[24]],A.prob[[25]],
                   A.prob[[26]],A.prob[[27]],A.prob[[28]],A.prob[[29]],A.prob[[30]],
                   A.prob[[31]],A.prob[[32]],A.prob[[33]],A.prob[[34]],A.prob[[35]],
                   A.prob[[36]],A.prob[[37]],A.prob[[38]],A.prob[[39]],A.prob[[40]]) %>% ggplot
ggdf + 
  geom_line(aes(x=A.theta,y=A.prob[[1]] ), color="red", linetype = 2, size = 0.8) +
  geom_line(aes(x=A.theta,y=A.prob[[2]] ), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[3]] ), color="blue", linetype = 4, size = 0.8) +
  geom_line(aes(x=A.theta,y=A.prob[[4]] ), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[5]] ), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[6]] ), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[7]] ), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[8]] ), color="red", linetype = 2, size = 0.8) +
  geom_line(aes(x=A.theta,y=A.prob[[9]] ), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[10]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[11]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[12]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[13]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[14]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[15]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[16]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[17]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[18]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[19]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[20]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[21]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[22]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[23]]), color="blue", linetype = 4, size = 0.8) +
  geom_line(aes(x=A.theta,y=A.prob[[24]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[25]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[26]]), color="red", linetype = 2, size = 0.8) +
  geom_line(aes(x=A.theta,y=A.prob[[27]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[28]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[29]]), color="red", linetype = 2, size = 0.8) +
  geom_line(aes(x=A.theta,y=A.prob[[30]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[31]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[32]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[33]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[34]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[35]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[36]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[37]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[38]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[39]]), color="black", linetype = 1, size = 0.5) +
  geom_line(aes(x=A.theta,y=A.prob[[40]]), color="black", linetype = 1, size = 0.5) +
  theme_classic(base_size = 15) + ylab("反応確率") + xlab("特性値 θ") +
  annotate("text", label="困難度の特に高い4項目(点線)", x=2.5, y=0.4, size=6) +
  annotate("text", label="困難度の特に低い2項目(点線)", x=-2.5, y=0.8, size=6) 


################################################################################
##### 
##### DIF item response model
##### 
################################################################################

ip.gender.model <- stan_model("model/IR2PL.Gender.BF.mac.stan")
ip.gender.res <- sampling(ip.gender.model,ip.dat,
                          init='random',
                          iter=5500,warmup=500,chain=4,seed=1234,thin=10)
gc(t,t);gc(t,t)

stan_rhat(ip.gender.res)
stan_trace(ip.gender.res)
stan_ess(ip.gender.res)
stan_ac(ip.gender.res)
stan_dens(ip.gender.res,separate_chains = T)

################################################################################
##### 
##### figure 04
##### 
################################################################################

G.alpha <- get_posterior_mean(ip.gender.res,"alpha")[,5]
G.beta <- get_posterior_mean(ip.gender.res,"beta")[,5]
M.alpha <- G.alpha[1:40]
W.alpha <- G.alpha[41:80]
M.beta <- G.beta[1:40]
W.beta <- G.beta[41:80]

M.alpha.pm <- data.frame(M.alpha,W.alpha)
M.alpha.pm <- data.frame(t(M.alpha.pm))
colnames(M.alpha.pm) <- paste("Q",1:40,sep = "")
round(M.alpha.pm[,1:40],2)

M.beta.pm <- data.frame(M.beta,W.beta)
M.beta.pm <- data.frame(t(M.beta.pm))
colnames(M.beta.pm) <- paste("Q",1:40,sep = "")
round(M.beta.pm[,1:40],2)


M.theta <- seq(-4,4,length.out=201)
M.prob <- vector("list",40)
M.prob[[1]] <- logistic(M.alpha[1]*(M.theta-M.beta[1]))
M.prob[[2]] <- logistic(M.alpha[2]*(M.theta-M.beta[2]))
M.prob[[3]] <- logistic(M.alpha[3]*(M.theta-M.beta[3]))
M.prob[[4]] <- logistic(M.alpha[4]*(M.theta-M.beta[4]))
M.prob[[5]] <- logistic(M.alpha[5]*(M.theta-M.beta[5]))
M.prob[[6]] <- logistic(M.alpha[6]*(M.theta-M.beta[6]))
M.prob[[7]] <- logistic(M.alpha[7]*(M.theta-M.beta[7]))
M.prob[[8]] <- logistic(M.alpha[8]*(M.theta-M.beta[8]))
M.prob[[9]] <- logistic(M.alpha[9]*(M.theta-M.beta[9]))
M.prob[[10]] <- logistic(M.alpha[10]*(M.theta-M.beta[10]))
M.prob[[11]] <- logistic(M.alpha[11]*(M.theta-M.beta[11]))
M.prob[[12]] <- logistic(M.alpha[12]*(M.theta-M.beta[12]))
M.prob[[13]] <- logistic(M.alpha[13]*(M.theta-M.beta[13]))
M.prob[[14]] <- logistic(M.alpha[14]*(M.theta-M.beta[14]))
M.prob[[15]] <- logistic(M.alpha[15]*(M.theta-M.beta[15]))
M.prob[[16]] <- logistic(M.alpha[16]*(M.theta-M.beta[16]))
M.prob[[17]] <- logistic(M.alpha[17]*(M.theta-M.beta[17]))
M.prob[[18]] <- logistic(M.alpha[18]*(M.theta-M.beta[18]))
M.prob[[19]] <- logistic(M.alpha[19]*(M.theta-M.beta[19]))
M.prob[[20]] <- logistic(M.alpha[20]*(M.theta-M.beta[20]))
M.prob[[21]] <- logistic(M.alpha[21]*(M.theta-M.beta[21]))
M.prob[[22]] <- logistic(M.alpha[22]*(M.theta-M.beta[22]))
M.prob[[23]] <- logistic(M.alpha[23]*(M.theta-M.beta[23]))
M.prob[[24]] <- logistic(M.alpha[24]*(M.theta-M.beta[24]))
M.prob[[25]] <- logistic(M.alpha[25]*(M.theta-M.beta[25]))
M.prob[[26]] <- logistic(M.alpha[26]*(M.theta-M.beta[26]))
M.prob[[27]] <- logistic(M.alpha[27]*(M.theta-M.beta[27]))
M.prob[[28]] <- logistic(M.alpha[28]*(M.theta-M.beta[28]))
M.prob[[29]] <- logistic(M.alpha[29]*(M.theta-M.beta[29]))
M.prob[[30]] <- logistic(M.alpha[30]*(M.theta-M.beta[30]))
M.prob[[31]] <- logistic(M.alpha[31]*(M.theta-M.beta[31]))
M.prob[[32]] <- logistic(M.alpha[32]*(M.theta-M.beta[32]))
M.prob[[33]] <- logistic(M.alpha[33]*(M.theta-M.beta[33]))
M.prob[[34]] <- logistic(M.alpha[34]*(M.theta-M.beta[34]))
M.prob[[35]] <- logistic(M.alpha[35]*(M.theta-M.beta[35]))
M.prob[[36]] <- logistic(M.alpha[36]*(M.theta-M.beta[36]))
M.prob[[37]] <- logistic(M.alpha[37]*(M.theta-M.beta[37]))
M.prob[[38]] <- logistic(M.alpha[38]*(M.theta-M.beta[38]))
M.prob[[39]] <- logistic(M.alpha[39]*(M.theta-M.beta[39]))
M.prob[[40]] <- logistic(M.alpha[40]*(M.theta-M.beta[40]))

W.theta <- seq(-4,4,length.out=201)
W.prob <- vector("list",40)
W.prob[[1]] <- logistic(W.alpha[1]*(W.theta-W.beta[1]))
W.prob[[2]] <- logistic(W.alpha[2]*(W.theta-W.beta[2]))
W.prob[[3]] <- logistic(W.alpha[3]*(W.theta-W.beta[3]))
W.prob[[4]] <- logistic(W.alpha[4]*(W.theta-W.beta[4]))
W.prob[[5]] <- logistic(W.alpha[5]*(W.theta-W.beta[5]))
W.prob[[6]] <- logistic(W.alpha[6]*(W.theta-W.beta[6]))
W.prob[[7]] <- logistic(W.alpha[7]*(W.theta-W.beta[7]))
W.prob[[8]] <- logistic(W.alpha[8]*(W.theta-W.beta[8]))
W.prob[[9]] <- logistic(W.alpha[9]*(W.theta-W.beta[9]))
W.prob[[10]] <- logistic(W.alpha[10]*(W.theta-W.beta[10]))
W.prob[[11]] <- logistic(W.alpha[11]*(W.theta-W.beta[11]))
W.prob[[12]] <- logistic(W.alpha[12]*(W.theta-W.beta[12]))
W.prob[[13]] <- logistic(W.alpha[13]*(W.theta-W.beta[13]))
W.prob[[14]] <- logistic(W.alpha[14]*(W.theta-W.beta[14]))
W.prob[[15]] <- logistic(W.alpha[15]*(W.theta-W.beta[15]))
W.prob[[16]] <- logistic(W.alpha[16]*(W.theta-W.beta[16]))
W.prob[[17]] <- logistic(W.alpha[17]*(W.theta-W.beta[17]))
W.prob[[18]] <- logistic(W.alpha[18]*(W.theta-W.beta[18]))
W.prob[[19]] <- logistic(W.alpha[19]*(W.theta-W.beta[19]))
W.prob[[20]] <- logistic(W.alpha[20]*(W.theta-W.beta[20]))
W.prob[[21]] <- logistic(W.alpha[21]*(W.theta-W.beta[21]))
W.prob[[22]] <- logistic(W.alpha[22]*(W.theta-W.beta[22]))
W.prob[[23]] <- logistic(W.alpha[23]*(W.theta-W.beta[23]))
W.prob[[24]] <- logistic(W.alpha[24]*(W.theta-W.beta[24]))
W.prob[[25]] <- logistic(W.alpha[25]*(W.theta-W.beta[25]))
W.prob[[26]] <- logistic(W.alpha[26]*(W.theta-W.beta[26]))
W.prob[[27]] <- logistic(W.alpha[27]*(W.theta-W.beta[27]))
W.prob[[28]] <- logistic(W.alpha[28]*(W.theta-W.beta[28]))
W.prob[[29]] <- logistic(W.alpha[29]*(W.theta-W.beta[29]))
W.prob[[30]] <- logistic(W.alpha[30]*(W.theta-W.beta[30]))
W.prob[[31]] <- logistic(W.alpha[31]*(W.theta-W.beta[31]))
W.prob[[32]] <- logistic(W.alpha[32]*(W.theta-W.beta[32]))
W.prob[[33]] <- logistic(W.alpha[33]*(W.theta-W.beta[33]))
W.prob[[34]] <- logistic(W.alpha[34]*(W.theta-W.beta[34]))
W.prob[[35]] <- logistic(W.alpha[35]*(W.theta-W.beta[35]))
W.prob[[36]] <- logistic(W.alpha[36]*(W.theta-W.beta[36]))
W.prob[[37]] <- logistic(W.alpha[37]*(W.theta-W.beta[37]))
W.prob[[38]] <- logistic(W.alpha[38]*(W.theta-W.beta[38]))
W.prob[[39]] <- logistic(W.alpha[39]*(W.theta-W.beta[39]))
W.prob[[40]] <- logistic(W.alpha[40]*(W.theta-W.beta[40]))

c.p <- NULL
for (i in 1:40) {
  c.p <- data.frame(W.theta,M.theta,
                          W.prob[[i]],M.prob[[i]]) %>% ggplot
  c.p <- c.p + 
    geom_line(aes(x=M.theta,y=M.prob[[i]] ), color="red", linetype = 2, size = 0.5) +
    geom_line(aes(x=W.theta,y=W.prob[[i]] ), color="black", linetype = 1, size = 0.5) +
    theme_classic(base_size = 15) + ylab("反応確率 (Qi)") + xlab("特性値 θ") +
    theme(plot.title = element_text(size=15),legend.position = "top")+
    annotate("text", label=paste("item=",i,sep = ""), x=2.5, y=0.2, size=6)
  ggsave(paste("plot/GEN",i,".png",sep = ""),c.p)
}

c.p <- vector("list",40)
c.p[[19]] <- data.frame(W.theta,M.theta,
                         W.prob[[19]],M.prob[[19]]) %>% ggplot
c.p[[19]] <- c.p[[19]] + 
  geom_line(aes(x=M.theta,y=M.prob[[19]] ), color="red", linetype = 2, size = 0.5) +
  geom_line(aes(x=W.theta,y=W.prob[[19]] ), color="black", linetype = 1, size = 0.5) +
  theme_classic(base_size = 15) + ylab("反応確率 (Q19)") + xlab("特性値 θ") +
  theme(plot.title = element_text(size=15),legend.position = "top")

c.p[[17]] <- data.frame(W.theta,M.theta,
                         W.prob[[17]],M.prob[[17]]) %>% ggplot
c.p[[17]] <- c.p[[17]] + 
  geom_line(aes(x=M.theta,y=M.prob[[17]] ), color="red", linetype = 2, size = 0.5) +
  geom_line(aes(x=W.theta,y=W.prob[[17]] ), color="black", linetype = 1, size = 0.5) +
  theme_classic(base_size = 15) + ylab("反応確率 (Q17)") + xlab("特性値 θ") +
  theme(plot.title = element_text(size=15),legend.position = "top")

c.p[[36]] <- data.frame(W.theta,M.theta,
                         W.prob[[36]],M.prob[[36]]) %>% ggplot
c.p[[36]] <- c.p[[36]] + 
  geom_line(aes(x=M.theta,y=M.prob[[36]] ), color="red", linetype = 2, size = 0.5) +
  geom_line(aes(x=W.theta,y=W.prob[[36]] ), color="black", linetype = 1, size = 0.5) +
  theme_classic(base_size = 15) + ylab("反応確率 (Q36)") + xlab("特性値 θ") +
  theme(plot.title = element_text(size=15),legend.position = "top")

c.p[[23]] <- data.frame(W.theta,M.theta,
                         W.prob[[23]],M.prob[[23]]) %>% ggplot
c.p[[23]] <- c.p[[23]] + 
  geom_line(aes(x=M.theta,y=M.prob[[23]] ), color="red", linetype = 2, size = 0.5) +
  geom_line(aes(x=W.theta,y=W.prob[[23]] ), color="black", linetype = 1, size = 0.5) +
  theme_classic(base_size = 15) + ylab("反応確率 (Q23)") + xlab("特性値 θ") +
  theme(plot.title = element_text(size=15),legend.position = "top")

c.p[[11]] <- data.frame(W.theta,M.theta,
                         W.prob[[11]],M.prob[[11]]) %>% ggplot
c.p[[11]] <- c.p[[11]] + 
  geom_line(aes(x=M.theta,y=M.prob[[11]] ), color="red", linetype = 2, size = 0.5) +
  geom_line(aes(x=W.theta,y=W.prob[[11]] ), color="black", linetype = 1, size = 0.5) +
  theme_classic(base_size = 15) + ylab("反応確率 (Q11)") + xlab("特性値 θ") +
  theme(plot.title = element_text(size=15),legend.position = "top")

c.p[[18]] <- data.frame(W.theta,M.theta,
                         W.prob[[18]],M.prob[[18]]) %>% ggplot
c.p[[18]] <- c.p[[18]] + 
  geom_line(aes(x=M.theta,y=M.prob[[18]] ), color="red", linetype = 2, size = 0.5) +
  geom_line(aes(x=W.theta,y=W.prob[[18]] ), color="black", linetype = 1, size = 0.5) +
  theme_classic(base_size = 15) + ylab("反応確率 (Q18)") + xlab("特性値 θ") +
  theme(plot.title = element_text(size=15),legend.position = "top")


grid.newpage() #空の画面を作る
pushViewport(viewport(layout=grid.layout(3, 2))) #画面を区切る（今回は3行2列の6分割）
print(c.p[[19]], vp=viewport(layout.pos.row=1, layout.pos.col=1)  ) # 1行目の1列
print(c.p[[17]], vp=viewport(layout.pos.row=1, layout.pos.col=2)  ) # 1行目の2列
print(c.p[[36]], vp=viewport(layout.pos.row=2, layout.pos.col=1)  ) # 2行目の1列
print(c.p[[23]], vp=viewport(layout.pos.row=2, layout.pos.col=2)  ) # 2行目の2列
print(c.p[[11]], vp=viewport(layout.pos.row=3, layout.pos.col=1)  ) # 3行目の1列
print(c.p[[18]], vp=viewport(layout.pos.row=3, layout.pos.col=2)  ) # 3行目の2列

# save.image("Rdata/irmodel.Rdata",compress = "xz") ### ファイルサイズかなり大きくなります。ご注意を

