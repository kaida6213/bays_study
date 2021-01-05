#Stan2.17, rstan 2.17.3, R3.4.4, Mac OS Sierraで動作確認済

#データの読み込み(rtは反応時間、subは参加者ID、cond=1は探索メインブロック、2は記憶メインブロック)
total_dat01 <- read.csv("search_rt.csv",header=T)

#外れ値を除外
total_dat01 <- subset(total_dat01,rt>0.20 & rt<15.00)

#参加者の条件ごとに平均値を算出し、対応のあるt検定(伝統的な方法)
total_mean <- tapply(total_dat01$rt,list(total_dat01$sub,total_dat01$cond),mean)
t.test(total_mean[,1],total_mean[,2],paired=T)

#stanの実行
library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())
datastan <- list(N = length(total_dat01$rt), RT=total_dat01$rt, SUBID = total_dat01$sub,CONDID = total_dat01$cond,S = length(sub_num))

#以下の２つの理由からadapt_deltaとmax_treedepthを大きめに設定しています。
#(1)divergent transitionsが多い
#(2)maximum treedepthのwarningがでる
#ただし、それでもdivergent transitionsはでます。
fit_exgauss01<-stan(file = 'exGauss_fit.stan', data = datastan, seed = 123, iter = 13000, warmup = 1000,chains = 4,thin = 3,control=list(adapt_delta=0.99,max_treedepth=15))

#stanの結果の要約情報
sum_dat <- summary(fit_exgauss01)$summary

#個人のパラメタの平均、2.5%点、97.5%点を取り出す
total_mu_ind <- cbind(sum_dat[paste("mu_ind[",1:10,",1]",sep = ""),c("mean","2.5%","97.5%")],
                      sum_dat[paste("mu_ind[",1:10,",2]",sep = ""),c("mean","2.5%","97.5%")])
total_sigma_ind <- cbind(sum_dat[paste("sigma_ind[",1:10,",1]",sep = ""),c("mean","2.5%","97.5%")],
                         sum_dat[paste("sigma_ind[",1:10,",2]",sep = ""),c("mean","2.5%","97.5%")])
total_lambda_ind <- cbind(sum_dat[paste("lambda_ind[",1:10,",1]",sep = ""),c("mean","2.5%","97.5%")],
                          sum_dat[paste("lambda_ind[",1:10,",2]",sep = ""),c("mean","2.5%","97.5%")])

#表15.1のデータ（集団レベルのパラメタ）
table1_dat <- rbind(sum_dat["mu[1]",c("mean","sd","2.5%","97.5%")],
                    sum_dat["mu[2]",c("mean","sd","2.5%","97.5%")],
                    sum_dat["sigma[1]",c("mean","sd","2.5%","97.5%")],
                    sum_dat["sigma[2]",c("mean","sd","2.5%","97.5%")],
                    sum_dat["lambda[1]",c("mean","sd","2.5%","97.5%")],
                    sum_dat["lambda[2]",c("mean","sd","2.5%","97.5%")])
table1_dat <- cbind(table1_dat,(table1_dat[,4]-table1_dat[,3]))
colnames(table1_dat) <- c("EAP","post.sd","2.50%","97.50%","95%幅")
rownames(table1_dat) <- c("mu_s","mu_m","sigma_s","sigma_m","lambda_s","lambda_m")

#表15.2のデータ（記憶メインブロックの個人パラメタ）
table2_dat <- cbind(total_mu_ind[,4],total_mu_ind[,6]-total_mu_ind[,5],
                    total_sigma_ind[,4],total_sigma_ind[,6]-total_sigma_ind[,5],
                    total_lambda_ind[,4],total_lambda_ind[,6]-total_lambda_ind[,5])
colnames(table2_dat) <- c("mu_eap","mu_95ci","sigma_eap","sigma_95ci","lambda_eap","lambda_95ci")
rownames(table2_dat) <- paste("s",1:10,sep="")

#表15.3のデータ（探索メインブロックの個人パラメタ）
table3_dat <- cbind(total_mu_ind[,1],total_mu_ind[,3]-total_mu_ind[,2],
                    total_sigma_ind[,1],total_sigma_ind[,3]-total_sigma_ind[,2],
                    total_lambda_ind[,1],total_lambda_ind[,3]-total_lambda_ind[,2])
colnames(table3_dat) <- c("mu_eap","mu_95ci","sigma_eap","sigma_95ci","lambda_eap","lambda_95ci")
rownames(table3_dat) <- paste("s",1:10,sep="")

#表15.4のデータ（差の分布）
table4_dat <- rbind(sum_dat["mu_diff",c("mean","sd","2.5%","97.5%")],
                    sum_dat["sigma_diff",c("mean","sd","2.5%","97.5%")],
                    sum_dat["lambda_diff",c("mean","sd","2.5%","97.5%")])
table4_dat <- cbind(table4_dat,(table4_dat[,4]-table4_dat[,3]))
colnames(table4_dat) <- c("EAP","post.sd","2.50%","97.5%","95%幅")
rownames(table4_dat) <- c("mu_diff","sigma_diff","lambda_diff")

###グラフの作成
library(ggplot2)

#ヒストグラムの作成(図15.3)
search_rt <- total_dat01$rt[total_dat01$cond==1]
memory_rt <- total_dat01$rt[total_dat01$cond==2]
cond_name <- c(rep("search-frequent",length(search_rt)), rep("memory-frequent",length(memory_rt)))
total_block <- data.frame(c(search_rt,memory_rt),cond_name)
names(total_block)<-c("rt","cond_name")

g <- ggplot() + theme_bw(base_size = 20) +
  theme(panel.background = element_rect(fill = "transparent", colour = "black"),panel.grid = element_blank()) +
  xlab("RT(sec)") + ylab("density") + 
  geom_histogram(data = total_block,aes(x = rt, y = ..density..)) + facet_grid(~cond_name)
g


#図15.1の作成（ただし、凡例はついていません）
library(retimes)
library(ggplot2)
library(cowplot)

x <- seq(0,3.5,0.01)
#retimesパッケージのdexgaussでは、λではなくτ(tau)を使用します。
#tau(τ)はλの逆数です。
#図15.1a
dat1 <- dexgauss(x,mu=0.5,sigma=0.1,tau=2.0^-1)
dat2 <- dexgauss(x,mu=1.0,sigma=0.1,tau=2.0^-1)
dat3 <- dexgauss(x,mu=1.5,sigma=0.1,tau=2.0^-1)

gdat <- data.frame(rep("A",length(x)*3),rep(x,3),c(rep("1",length(dat1)),rep("2",length(dat1)),rep("3",length(dat1))),c(dat1,dat2,dat3))
names(gdat) <- c("type","rt","cond","ds")

#図15.1b
dat4 <- dexgauss(x,mu=0.8,sigma=0.1,tau=2.0^-1)
dat5 <- dexgauss(x,mu=0.8,sigma=0.3,tau=2.0^-1)
dat6 <- dexgauss(x,mu=0.8,sigma=0.7,tau=2.0^-1)
gdat02 <- data.frame(rep("B",length(x)*3),rep(x,3),c(rep("1",length(dat4)),rep("2",length(dat5)),rep("3",length(dat6))),c(dat4,dat5,dat6))
names(gdat02) <- c("type","rt","cond","ds")

#図15.1c
dat7 <- dexgauss(x,mu=0.8,sigma=0.1,tau=1.0^-1)
dat8 <- dexgauss(x,mu=0.8,sigma=0.1,tau=2.0^-1)
dat9 <- dexgauss(x,mu=0.8,sigma=0.1,tau=3.0^-1)
gdat03 <- data.frame(rep("C",length(x)*3),rep(x,3),c(rep("1",length(dat4)),rep("2",length(dat5)),rep("3",length(dat6))),c(dat7,dat8,dat9))
names(gdat03) <- c("type","rt","cond","ds")

#図15.1d
dat10 <- dexgauss(x,mu=0.5,sigma=0.1,tau=1.0^-1)
dat11 <- dexgauss(x,mu=1.0,sigma=0.1,tau=2.0^-1)
gdat04 <- data.frame(rep("D",length(x)*2),rep(x,2),c(rep("1",length(dat4)),rep("2",length(dat5))),c(dat10,dat11))
names(gdat04) <- c("type","rt","cond","ds")

tdat <- rbind(gdat,gdat02,gdat03,gdat04)

g2 <- ggplot() +
  geom_line(data = tdat,aes(x = rt,y = ds, linetype=cond),size=0.6) +
  xlab("RT(sec)")+ ylab("density")+ 
  theme_bw(base_size = 17) + theme(panel.background = element_rect(fill = "transparent", colour = "black"),panel.grid = element_blank()) + ylim(0,2.0) +
  facet_wrap(~tdat$type,ncol = 2)
g2
