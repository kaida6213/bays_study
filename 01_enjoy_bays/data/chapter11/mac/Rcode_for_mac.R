# パッケージ読み込み----
library(ggplot2)
old = theme_set(theme_gray(base_family="HiraKakuProN-W3"))
library(tidyverse)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# データ読み込み----
dat01 <- read.csv("m1_score.csv", fileEncoding = "CP932")

# 審査員，演者，年代について重複を取り除き取り出しておく(可視化の際にも使用)
r_name <- unique(dat01$審査員)
p_name <- unique(dat01$演者)
y_name <- unique(dat01$年代)

# Stan用データの準備----
## model1 コンビ平均モデル
datastan01 <- list(L = nrow(dat01), # サンプルサイズ
                   N = max(as.numeric(dat01$演者)), # 演者に数をふり，最大値を持ってきて演者数を入れる
                   idX = as.numeric(dat01$演者), # 演者に数をふり，グループ変数のように扱う
                   Y = as.numeric(dat01$val_z) # 各漫才の得点を標準化した得点
)

## model2 審査員のくせ評価モデル, model3 審査員の基準効果モデル
datastan02 <- list(L = nrow(dat01),
                   N = max(as.numeric(dat01$演者)),
                   M = max(as.numeric(dat01$審査員)),
                   idX = as.numeric(dat01$演者),
                   idY = as.numeric(dat01$審査員),
                   Y = as.numeric(dat01$val_z)
)

## model4 開催回数効果モデル
datastan04 <- list(L = nrow(dat01),
                   N = max(as.numeric(dat01$演者)),
                   M = max(as.numeric(dat01$審査員)),
                   O = max(as.numeric(dat01$年代)),
                   idX = as.numeric(dat01$演者),
                   idY = as.numeric(dat01$審査員),
                   idZ = as.numeric(dat01$年代),
                   Y = as.numeric(dat01$val_z)
)

# model1　コンビ平均モデル----
model01 <- stan_model("model01.stan") # model01.stanのファイルをコンパイルし，model01として読み込む
fit01 <- sampling(model01, # model01のモデルを使用
                  data = datastan01, # datastan01のデータを使用
                  iter = 10000, # 1チェインのサンプリング回数
                  chains = 4 # チェインの数
                  )

## result01
fit01

## theta result コンビ平均モデルのおもしろさの推定結果
fit01_theta <- rstan::extract(fit01)$theta %>% transform() # thetaに関する推定結果を抽出

pl_df_m <- data.frame(list(
  EAP = apply(fit01_theta, 2, mean),   # meanを算出
  post.sd = apply(fit01_theta, 2, sd), # sdを算出
  下限 = apply(fit01_theta, 2, function(x) quantile(x, probs = 0.025)), # 95%信用区間の下限(2.5%)を算出
  上限 = apply(fit01_theta, 2, function(x) quantile(x, probs = 0.975))  # 95%信用区間の上限(97.5%)を算出
))

pl_df_m <- round(pl_df_m, digits = 2)  # 小数点以下を2桁で丸める
pl_df_m$name <- factor(levels(p_name)) # 推定結果のデータフレームにコンビ名の列を加える

pl_df_m01 <- pl_df_m %>% 
  dplyr::arrange(desc(EAP)) %>%     # 降順で並び替える
  dplyr::select(name, everything()) # 見やすいようにコンビ名のラベルを左へ移動
pl_df_m01

## top 5
pl_df_m01 %>% 
  dplyr::top_n(5, EAP) %>% # EAPの推定値のトップ5を抽出
  dplyr::arrange(desc(EAP)) %>% 
  dplyr::select(name, everything())

## sigma result　コンビ平均モデルの標準偏差の推定結果
fit01_sig <- rstan::extract(fit01)$sig %>% transform()

pl_df_sig <- data.frame(list(
  median = apply(fit01_sig, 2, function(x) quantile(x, probs = 0.50)),
  L95 = apply(fit01_sig, 2, function(x) quantile(x, probs = 0.025)),
  U95 = apply(fit01_sig, 2, function(x) quantile(x, probs = 0.975))
))

pl_df_sig <- round(pl_df_sig, digits = 2)
pl_df_sig$name <- factor(levels(p_name))

### おもしろさのばらつきが大きい上位3組をまとめる
pl_df_sig01 <- pl_df_sig %>% 
  dplyr::top_n(3, median) %>%
  dplyr::arrange(desc(median)) %>% 
  dplyr::select(name, everything())
### おもしろさのばらつきが小さい下位3組をまとめる
pl_df_sig02 <- pl_df_sig %>% 
  dplyr::top_n(-3, median) %>%
  dplyr::arrange(median) %>% 
  dplyr::select(name, everything())
### ばらつき上位3組と下位組をまとめる
pl_df_sig03 <- dplyr::union(pl_df_sig01, pl_df_sig02) %>% 
  dplyr::arrange(desc(median))
pl_df_sig03

# model2　審査員のくせ評価モデル----
model02 <- stan_model("model02.stan")
fit02 <- sampling(model02,
                  data = datastan02,
                  iter = 10000,
                  chains = 4,
                  seed = 1235
                  )

## result02
fit02

## theta result02　審査員のくせ評価モデルのおもしろさの推定結果
fit02_theta <- rstan::extract(fit02)$theta %>% transform()

pl_df02 <- data.frame(list(
  EAP = apply(fit02_theta, 2, mean),
  post.sd = apply(fit02_theta, 2, sd),
  下限 = apply(fit02_theta, 2, function(x) quantile(x, probs = 0.025)),
  上限 = apply(fit02_theta, 2, function(x) quantile(x, probs = 0.975))
))

pl_df02 <- round(pl_df02, digits = 2)
pl_df02$name <- factor(levels(p_name))

pl_df02 %>% 
  dplyr::top_n(5, EAP) %>%
  dplyr::arrange(desc(EAP)) %>% 
  dplyr::select(name, everything())

## sigma result02　審査員のくせ評価モデルの標準偏差の推定結果
fit02_sig <- rstan::extract(fit02)$sig %>% transform()

r_df02 <- data.frame(list(
  MED = apply(fit02_sig,2,function(x) quantile(x,probs=0.50)),
  post.sd = apply(fit02_sig,2,sd),
  下限 = apply(fit02_sig, 2, function(x) quantile(x, probs = 0.025)),
  上限 = apply(fit02_sig, 2, function(x) quantile(x, probs = 0.975))
  )
  )

r_df02 <- round(r_df02, digits = 2)
r_df02$name <- factor(levels(r_name))

r_df02_top <- r_df02 %>% 
  dplyr::top_n(3, MED) %>%
  dplyr::arrange(desc(MED)) %>% 
  dplyr::select(name, everything())

r_df02_btm <- r_df02 %>% 
  dplyr::top_n(-3, MED) %>%
  dplyr::arrange(MED) %>% 
  dplyr::select(name, everything())

r_df02_union <- dplyr::union(r_df02_top, r_df02_btm)
r_df02_union %>% 
  dplyr::arrange(desc(MED))

# model3　審査員の基準効果モデル----
model03 <- stan_model("model03.stan")
fit03 <- sampling(model03,
                  data = datastan02,
                  iter = 10000,
                  chains = 4,
                  seed = 1235
                  )

## results
fit03

## theta results　審査員の基準効果モデルのおもしろさの推定結果
fit03_theta <- rstan::extract(fit03)$theta %>% transform()

pl_df03 <- data.frame(list(
  EAP = apply(fit03_theta, 2, mean),
  post.sd = apply(fit03_theta, 2, sd),
  下限 = apply(fit03_theta, 2, function(x) quantile(x, probs = 0.025)),
  上限 = apply(fit03_theta, 2, function(x) quantile(x, probs = 0.975))
  ))

pl_df03 <- round(pl_df03, digits = 2)
pl_df03$name <- factor(levels(p_name))

pl_df03 %>% 
  dplyr::top_n(5, EAP) %>%
  dplyr::arrange(desc(EAP)) %>% 
  dplyr::select(name, everything())

## gamma results　審査員の基準効果モデルの審査基準の推定結果
fit03_gamma <- rstan::extract(fit03)$gamma %>% transform()

r_df03 <- data.frame(list(
  EAP = apply(fit03_gamma,2,mean),
  post.sd = apply(fit03_gamma,2,sd),
  下限 = apply(fit03_gamma, 2, function(x) quantile(x, probs = 0.025)),
  上限 = apply(fit03_gamma, 2, function(x) quantile(x, probs = 0.975))
  ))

r_df03 <- round(r_df03, digits = 2)
r_df03$name <- factor(levels(r_name))

r_df03_top <- r_df03 %>% 
  dplyr::top_n(3, EAP) %>%
  dplyr::arrange(desc(EAP)) %>% 
  dplyr::select(name, everything())

r_df03_btm <- r_df03 %>% 
  dplyr::top_n(-3, EAP) %>%
  dplyr::arrange(EAP) %>% 
  dplyr::select(name, everything())

r_df03_union <- dplyr::union(r_df03_top, r_df03_btm)
r_df03_union %>% 
  dplyr::arrange(desc(EAP))

## sigma results　審査員の基準効果モデルの標準偏差の推定結果
fit03_sig <- rstan::extract(fit03)$sig %>% transform()

pl_df03_sig <- data.frame(list(
  MED = apply(fit03_sig, 2, function(x) quantile(x, probs = 0.5)),
  post.sd = apply(fit03_sig, 2, sd),
  下限 = apply(fit03_sig, 2, function(x) quantile(x, probs = 0.025)),
  上限 = apply(fit03_sig, 2, function(x) quantile(x, probs = 0.975))
))

pl_df03_sig <- round(pl_df03_sig, digits = 2)
pl_df03_sig$name <- factor(levels(p_name))

pl_df03_sig %>% 
  dplyr::top_n(5, MED) %>%
  dplyr::arrange(desc(MED)) %>% 
  dplyr::select(name, everything())

# model4　開催回数効果モデル----
model04 <- stan_model("model04.stan")
fit04 <- sampling(model04,
                  data = datastan04,
                  iter = 10000,
                  chains = 4,
                  seed = 1235)

## results
fit04

### MCMCの収束の確認(Rhat <= 1.10)
all(summary(fit04)$summary[, "Rhat"] <= 1.10, na.rm = TRUE)

## theta results　開催回数効果モデルのおもしろさの推定結果
fit04_theta <- rstan::extract(fit04)$theta %>% transform()

pl_df04 <- data.frame(list(
  EAP = apply(fit04_theta, 2, mean),
  post.sd = apply(fit04_theta, 2, sd)
))

pl_df04 <- round(pl_df04, digits = 2)
pl_df04$name <- factor(levels(p_name))

pl_df04 %>% 
  dplyr::arrange(desc(EAP)) %>% 
  dplyr::select(name, everything())

## gamma results　開催回数効果モデルの審査基準の推定結果
fit04_gamma <- rstan::extract(fit04)$gamma %>% transform()

r_df04 <- data.frame(list(
  EAP = apply(fit04_gamma, 2, mean),
  post.sd = apply(fit04_gamma, 2, sd)
))

r_df04 <- round(r_df04, digits = 2)
r_df04$name <- factor(levels(r_name))

r_df04 %>% 
  dplyr::arrange(desc(EAP)) %>% 
  dplyr::select(name, everything())


## zeta results and plot(図1.1)　開催回数効果モデルの開催回数の推定結果
fit04_zeta <- rstan::extract(fit04)$zeta %>% transform()

y_df04 <- data.frame(list(
  EAP = apply(fit04_zeta,2,mean),
  post.sd = apply(fit04_zeta,2,sd),
  L95 = apply(fit04_zeta,2,function(x) quantile(x,probs=0.025)),
  U95 = apply(fit04_zeta,2,function(x) quantile(x,probs=0.975))
))

y_df04 <- round(y_df04, digits = 2)
y_df04$name <- levels(factor(y_name))
y_df04$name <- as.integer(y_df04$name)

### plot 図1.1の作成----
g <- ggplot(y_df04,aes(x=factor(name),y=EAP),colour="black") + geom_point(color="black")
g <- g + geom_errorbar(aes(ymax=U95,ymin=L95,width=0,alpha=0.8),colour="black")
g <- g + coord_flip()+ theme(legend.position = 'none')
g <- g + xlab("開催回数") + ylab("EAP推定値")
g <- g + theme(axis.text.y = element_text(size = 16),
               axis.title.y = element_text(size = 18), 
               axis.title.x = element_text(size = 18),
               axis.text.x = element_text(size = 16))
plot(g)
ggsave ("year_plot.pdf", units = "mm")

## M1の1回の大会の安定性(信頼性rho) 
round(summary(fit04)$summary["rho_theta", "mean"], digits = 2)

### plot2 図1.2の作成
fit04_rho <- rstan::extract(fit04)$rho_theta
rho_df <- data.frame(fit04_rho)
colnames(rho_df) <- "rho"

q_lower <- quantile(rho_df$rho, probs = 0.025) # 95%信用区間の下限を算出
q_upper <- quantile(rho_df$rho, probs = 0.975) # 95%信用区間の上限を算出
dens <- density(rho_df$rho)
dens_df <- data.frame(rho = dens$x, 確率密度 = dens$y)

g01 <- ggplot(data = dens_df, aes(x = rho, y = 確率密度))
g01 <- g01 + geom_line(size = 0.7)
g01 <- g01 + geom_ribbon(data = subset(dens_df,
                                       rho >= q_lower & rho <= q_upper),
                         aes(x = rho, ymin=0, ymax = 確率密度), fill = "black", alpha = 0.4
                         )
g01 <- g01 + labs(x = expression(italic(rho)[italic(theta)]))
g01 <- g01 + theme(axis.text = element_text(size = 16),
                   axis.title = element_text(size = 18)
                   )
g01
ggsave ("rho_dens.pdf", units = "mm")
