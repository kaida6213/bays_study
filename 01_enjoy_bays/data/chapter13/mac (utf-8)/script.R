# Stan2.17, rstan2.17.3, R3.4.3, mac OS High Sierra 10.13.6で動作確認済, 2018-09-04

dat <- read.csv("loafing_data.csv", as.is=T)

colnames(dat)
# id: 参加者の番号
# group: 参加者の所属する集団の番号
# cond: 実験操作の有無(0 = 統制、1 = 実験) -- 独立変数
# pre.wprk: 前半の作業量 -- 統制変数
# post.work: 後半の作業量 -- 従属変数
# peerN: 集団の人数 -- 統制変数

# 図2の箱ひげ図の作成
par(bty="l")
boxplot(subset(dat,dat$cond==0)$pre.work,
        subset(dat,dat$cond==1)$pre.work,
        subset(dat,dat$cond==0)$post.work,
        subset(dat,dat$cond==1)$post.work,
        names=c("前半-統制群","前半-実験群","後半-統制群","後半-実験群"),
        ylab="作業量")


# 級内相関係数の計算
# ここではICCパッケージ(https://github.com/matthewwolak/ICC)を使用
# 事前にinstall.packages("ICC")でパッケージのインストールが必要
library(ICC)
dat$group_f <- as.factor(dat$group)

# 前半作業量の級内相関係数
ICCbare(group_f, pre.work, dat)
# 後半作業量の級内相関係数
ICCbare(group_f, post.work, dat)


# 前半作業量を個人レベルと集団レベルに分離(集団中心化)
dat$pre.work_m <- tapply(dat$pre.work, dat$group,
                         mean, na.rm=T)[as.character(dat$group)]
dat$pre.work_c <- dat$pre.work - dat$pre.work_m

# 集団レベル変数の全体中心化
dat$pre.work_m_gm <- dat$pre.work_m - mean(dat$pre.work_m, na.rm=T)
dat$peerN_g <- dat$peerN - mean(dat$peerN, na.rm=T)

### パラメータの推定 ###
library(rstan)

# stanでの推定用にデータをリスト形式に変換
datastan <- list(N = nrow(dat),
                 G = length(unique(dat$group)),
                 post = dat$post.work,
                 pre = dat$pre.work_c,
                 preg = dat$pre.work_m_gm,
                 peerN = dat$peerN_g,
                 cond = dat$cond,
                 group = as.numeric(as.factor(dat$group)))

# モデルを記述したstanファイルの読み込み
model <- stan_model(file='model.stan')

# MCMCによる推定
fit <- sampling(model, data=datastan, seed=1234, iter=400000, warmup=200000)

# 主なパラメータの推定結果の確認
print(fit, pars=c("gamma_0","gamma0_",
                  "tau","sigma","es",
                  "lp__"),digits=3)

# 実験操作の効果(gamma_03)の収束を確認
stan_trace(fit, pars="gamma0_[3]")
stan_hist(fit, pars="gamma0_[3]")
stan_dens(fit, pars="gamma0_[3]", separate_chains = T)
stan_ac(fit, pars="gamma0_[3]", separate_chains = T)

# Q1. 実験操作の効果が0より小さいという仮説が正しい確率(Pr(gamma_03<0))を計算
ms_fit <- rstan::extract(fit)
sum(ms_fit$gamma0_[,3]<0)/length(ms_fit$gamma0_[,3])

# Q2. 効果量のEAPと95%確信区間を確認
mean(ms_fit$es)
quantile(ms_fit$es,probs=c(0.025,0.975))

# Q2. 割合のEAPと95%確信区間を確認
mean(ms_fit$rate)
quantile(ms_fit$rate,probs=c(0.025,0.975))
