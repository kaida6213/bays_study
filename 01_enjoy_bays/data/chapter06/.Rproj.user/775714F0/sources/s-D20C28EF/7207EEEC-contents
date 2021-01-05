# Stan2.17.0, rstan 2.17.2, Xcode 9.2 (9C40b), R3.4.3, MacOSで動作確認済

# 留意事項 -------------------------------------------------------------
#  本コードは第6章 章題『男心をくすぐる戦略』のRスクリプトです。
#  本文中の解析や図の出力に加え、要約統計量などの算出コードが含まれます。
# ----------------------------------------------------------------------


# 解析の準備------------------------------------------------------------

# パッケージのインストール
install.packages(c("rstan", "tidyverse", "formattable", "Rmisc", "ggmcmc", "shinystan", "loo"))

# パッケージの読み込み
library(rstan) # RStan
library(dplyr) # データ整形に利用
library(stringr) #文字列の処理に利用
library(ggplot2) # 図の描画に利用
library(formattable) # HTML表の出力
library(Rmisc) # 図を並べて表示する際に利用
library(ggmcmc) # 推定結果からMCMCサンプルを取り出し
library(shinystan) # 結果の確認やGelman-Rubin検定に利用
library(loo) # 情報量規準WAICの算出

# Stan並列化
options(mc.cores = parallel::detectCores())
# 同じモデルを再コンパイルしないように設定
rstan_options(auto_write = TRUE)


# データの読み込み -----------------------------------------------------
d <- read.csv("data.csv", fileEncoding = "shift-jis")

# データの確認
head(d)
str(d)

# ID: 実験参加者9名の識別ID
# C: 選択データ(後日のデートを選択した場合1)
# U: 「今日のデート」に登場した女性モデルに対する当該参加者の好み度評定値
# A: 「後日のデート」に登場した女性モデルに対する当該参加者の好み度評定値
# Delay: 後日のデートまでの遅延日数(本文中はtで表記)

# 要約統計量の算出(本文にはない) ---------------------------------------
# 全体
d %>%
  dplyr::select(-ID) %>%
  dplyr::group_by(Delay) %>%
  dplyr::summarise_all(funs(mean, sd)) %>%
  formattable()

# 個人ごと
d %>%
  dplyr::group_by(ID,Delay) %>%
  dplyr::summarise_all(funs(mean, sd))%>%
  formattable()


# ローデータの可視化 ---------------------------------------------------
# 可視化用データの整形
df_plot <- d %>% # データフレームの選択
  dplyr::select(ID, Delay, C) %>%
  dplyr::group_by(ID, Delay) %>% # IDと遅延日数ごとに処理を行う
  dplyr::mutate(ProbChooseLater = mean(C)) %>% # 選択率を算出
  dplyr::ungroup() %>%
  dplyr::group_by(Delay) %>% # 遅延日数ごとに処理を行う
  dplyr::mutate(WholeProbChooseLater = mean(C)) %>% # 選択率を算出
  dplyr::ungroup() %>%
  dplyr::select(-C) %>%
  # 今日の選択率は理論上0.5なのでそのデータを可視化用に追加
  rbind(data.frame(ID = seq(1:max(d$ID)),
                   Delay = rep(0),
                   ProbChooseLater = rep(0.5),
                   WholeProbChooseLater = rep(0.5))
        ) %>%
  dplyr::arrange(ID,Delay)

# 遅延選択肢の選択率(個人ごと)
g_1 <- ggplot(data = df_plot,
       mapping = aes(x = Delay, y = ProbChooseLater)) +
  geom_hline(yintercept = 0.5, linetype = 2, colour = "gray70") +
  geom_point(size = 0.5, colour = "grey30") +
  geom_line(size = 0.5, colour = "grey30") +
  facet_wrap(~ID) +
  theme_bw(base_size = 14, base_family = "HiraKakuProN-W3") + # Windowsの場合は、WindowsFonts()で確認して設定
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "デートまでの日数", y = "遅延選択肢の選択比率(個人ごと)") +
  theme(panel.grid.major = element_line(linetype = "blank"), 
        panel.grid.minor = element_line(linetype = "blank"))


# 遅延選択肢の選択率(全体平均)
g_2 <- ggplot(data = df_plot,
       mapping = aes(x = Delay, y = WholeProbChooseLater)) +
  geom_hline(yintercept = 0.5, linetype = 2, colour = "gray70") +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 14, base_family = "HiraKakuProN-W3") + # Windowsの場合は、WindowsFonts()で確認して設定
  labs(x = "デートまでの日数", y = "遅延選択肢の選択比率(全体)")


# 描画
Rmisc::multiplot(g_1, g_2, cols = 2)

# 図の保存
ggsave(Rmisc::multiplot(g_1, g_2, cols = 2),
       filename = "figures/figure2.png",
       dpi = 600,
       w = 9,
       h = 5.5)


# Stanに渡すデータの準備
N <- max(d$ID) # 参加者数
r <- nrow(d) # 縦持ちデータの行数
ID <- d$ID # 参加者ID識別子
C <- d$C # 選択結果(遅延選択肢 = 1)
U <- d$U # 今日の選択肢に登場したモデル女性の好み度評定値
A <- d$A # 後日の選択肢に登場したモデル女性の好み度評定値
Delay <- d$Delay # 遅延日数


# Stanデータ
datastan <- list(
  N = N,
  r = r,
  ID = ID,
  C = C,
  U = U,
  A = A,
  Delay = Delay
  )

# モデルシミュレーション-----------------------------------------------

# 初期値の設定
V <- 50 # モデルの好み度は50に固定
Delay <- c(1, 3, 7, 13, 30) # 遅延日数は実験内容と同一

#
# 1. 指数型割引モデル
#

Exp <- data.frame(Delay = c(0,Delay),
                  γ0.90 = c(50, V * 0.90^Delay),
                  γ0.93 = c(50, V * 0.93^Delay),
                  γ0.96 = c(50, V * 0.96^Delay)) %>% 
  tidyr::gather(key = Parameter, value = value, -Delay)

# 可視化
g_Exp <- ggplot(data = Exp,
                mapping = aes(x = Delay, y = value, group = Parameter, linetype = Parameter)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw(base_size = 12, base_family = "HiraKakuProN-W3") +  # Windowsの場合は、WindowsFonts()で確認して設定
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        legend.position = c(0.77, 0.8),
        legend.background = element_rect(colour = "black"),
        plot.title = element_text(size = 12)) +
  labs(title = "指数型割引モデル", x = "デートまでの日数", y = "デートの魅力")
g_Exp


#
# 2. 双極型割引モデル
#

Hyp <- data.frame(Delay = c(0, Delay),
                  γ0.90 = c(50, V /(1 + 0.90 * Delay)),
                  γ0.30 = c(50, V /(1 + 0.30 * Delay)),
                  γ0.10 = c(50, V /(1 + 0.10 * Delay))) %>%
  tidyr::gather(key = Parameter, value = value, -Delay)

# 可視化
g_Hyp <- ggplot(data = Hyp,
                mapping = aes(x = Delay, y = value, group = Parameter, linetype = Parameter)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw(base_size = 12, base_family = "HiraKakuProN-W3") +  # Windowsの場合は、WindowsFonts()で確認して設定
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        legend.position = c(0.77, 0.8),
        legend.background = element_rect(colour = "black"),
        plot.title = element_text(size = 12)) +
  labs(title = "双極型割引モデル", x = "デートまでの日数", y = "デートの魅力") 


#
# 3. わくわく定数加算モデル
#

Fixed_Savouring <- data.frame(Delay = c(0, Delay),
                              γ0.90_ω15 = c(50, 15 + V * 0.90^Delay),
                              γ0.93_ω20 = c(50, 20 + V * 0.93^Delay),
                              γ0.96_ω25 = c(50, 25 + V * 0.96^Delay)) %>%
  tidyr::gather(key = Parameter, value = value, -Delay)

# 可視化
g_FS <- ggplot(data = Fixed_Savouring,
               mapping = aes(x = Delay, y = value, group = Parameter, linetype = Parameter)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw(base_size = 12, base_family = "HiraKakuProN-W3") +  # Windowsの場合は、WindowsFonts()で確認して設定
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        legend.position = c(0.7, 0.8),
        legend.background = element_rect(colour = "black"),
        plot.title = element_text(size = 12))+
  labs(title = "わくわく定数加算モデル", x = "デートまでの日数", y = "デートの魅力") 



#
# 4. 総和型わくわく非割引モデル
#

UndiscountExpSavouring <- data.frame(Delay = c(0, Delay),
                                     γ0.90_α0.02 = c(50, 0.02 * V * (1 - 0.90^(Delay)) / (1 - 0.90) + V * 0.90^Delay),
                                     γ0.93_α0.03 = c(50, 0.03 * V * (1 - 0.93^(Delay)) / (1 - 0.93) + V * 0.93^Delay),
                                     γ0.96_α0.05 = c(50, 0.05 * V * (1 - 0.96^(Delay)) / (1 - 0.96) + V * 0.96^Delay)) %>%
  tidyr::gather(key = Parameter, value = value, -Delay)

# 可視化
g_UES <- ggplot(data = UndiscountExpSavouring,
                mapping = aes(x = Delay, y = value, group = Parameter, linetype = Parameter)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw(base_size = 12, base_family = "HiraKakuProN-W3") +  # Windowsの場合は、WindowsFonts()で確認して設定
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        legend.position = c(0.7, 0.8),
        legend.background = element_rect(colour = "black"),
        plot.title = element_text(size = 12))+
  labs(title = "総和型わくわく非割引モデル", x = "デートまでの日数", y = "デートの魅力") 


#
# 5. 制限付き総和型わくわく割引モデル
# 

RestrictedExpSavouring <- data.frame(Delay = c(0, Delay),
                                     γ0.90_α0.02 = c(50, 0.02 * V * 0.90^Delay * (1 - 0.90^(Delay)) / (1 - 0.90) + V * 0.90^Delay),
                                     γ0.93_α0.04 = c(50, 0.04 * V * 0.93^Delay * (1 - 0.93^(Delay)) / (1 - 0.93) + V * 0.93^Delay),
                                     γ0.96_α0.06 = c(50, 0.06 * V * 0.96^Delay * (1 - 0.96^(Delay)) / (1 - 0.96) + V * 0.96^Delay)) %>%
  tidyr::gather(key = Parameter, value = value, -Delay)

# 可視化
g_RES <- ggplot(data = RestrictedExpSavouring,
                mapping = aes(x = Delay, y = value, group = Parameter, linetype = Parameter)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw(base_size = 12, base_family = "HiraKakuProN-W3") +  # Windowsの場合は、WindowsFonts()で確認して設定
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        legend.position = c(0.7, 0.8),
        legend.background = element_rect(colour = "black"),
        plot.title = element_text(size = 12))+
  labs(title = "制限付き総和型わくわく割引モデル", x = "デートまでの日数", y = "デートの魅力")


#
# 6. 総和型わくわく割引モデル
#

DiscountExpSavouring <- data.frame(Delay = c(0, Delay),
                                   γs0.1_γd0.90_α0.9 = c(50, 0.9 * V * 0.1^Delay * (1 - 0.90^(Delay)) / (1 - 0.90) + V * 0.90^Delay),
                                   γs0.3_γd0.93_α0.8 = c(50, 0.8 * V * 0.3^Delay * (1 - 0.93^(Delay)) / (1 - 0.93) + V * 0.93^Delay),
                                   γs0.5_γd0.96_α0.7 = c(50, 0.7 * V * 0.5^Delay * (1 - 0.96^(Delay)) / (1 - 0.96) + V * 0.96^Delay)) %>%
  tidyr::gather(key = Parameter, value = value, -Delay)

# 可視化
g_DES <- ggplot(data = DiscountExpSavouring,
                mapping = aes(x = Delay, y = value, group = Parameter, linetype = Parameter)) +
  geom_line() +
  geom_point()  +
  theme_bw(base_size = 12, base_family = "HiraKakuProN-W3") +  # Windowsの場合は、WindowsFonts()で確認して設定
  scale_y_continuous(limits = c(0, 100)) +
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        legend.position = c(0.6, 0.8),
        legend.background = element_rect(colour = "black"),
        plot.title = element_text(size = 12)) +
  labs(title = "総和型わくわく割引モデル", x = "デートまでの日数", y = "デートの魅力")



# 全結果の描画
Rmisc::multiplot(g_Exp, g_Hyp, g_FS, g_UES, g_RES, g_DES, cols = 3)

# 保存
ggsave(Rmisc::multiplot(g_Exp, g_Hyp, g_FS, g_UES, g_RES, g_DES, cols = 3),
       w = 10,
       h = 8,
       dpi = 600,
       filename = "figures/figure3.png")




# 解析の実施 --------------------------------------------------------------

# モデル1 制限付き総和型わくわく割引モデル---------------------------------

# Stanモデルの読み込みとコンパイル
model1 <- stan_model("models/model_1.stan")
# saveRDS(model1, file = "models/model_1.rds") # コンパイル済みモデルを保存する場合
# model1 <- readRDS("models/model_1.rds") # コンパイル済みモデルを読み込む場合

# 推定の実行
fit1 <- sampling(model1,
                 data = datastan,
                 seed = 123,
                 iter = 4000,
                 warmup = 2000,
                 thin = 4,
                 control = list(adapt_delta = 0.9)
                 )

# 結果の保存
# save(fit1, file = "results/fit1.Rdata", compress = "xz")
# load("results/fit1.Rdata") # 結果を読み込む場合

# Gelman-Rubin検定 DIAGNOSEから確認する
# launch_shinystan(fit1)

# WAICとLOOCV
waic(loo::extract_log_lik(fit1))
loo(loo::extract_log_lik(fit1))


#
# サンプルの取り出しと可視化
#

#サンプルの取り出し
samples1 <- ggmcmc::ggs(fit1)

# 選択確率thetaの取り出しと整形
theta <- samples1 %>%
  dplyr::filter(str_detect(Parameter, "theta")) %>%
  dplyr::group_by(Parameter) %>%
  dplyr::summarise(EAP = mean(value)) %>%
  ungroup() %>%
  cbind(d,.) %>%
  dplyr::group_by(ID, Delay) %>%
  dplyr::mutate(ProbChooseLater = mean(C),
                IndividualEAP = mean(EAP)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Delay) %>%
  dplyr::mutate(WholeProbChooseLater = mean(C),
                WholeEAP = mean(EAP)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-C, -Parameter, -U, -A, -EAP) %>%
  rbind(data.frame(ID = seq(1:max(d$ID)),
                   Delay = rep(0),
                   ProbChooseLater = rep(0.5),
                   IndividualEAP = rep(0.5),
                   WholeProbChooseLater = rep(0.5),
                   WholeEAP = rep(0.5))) %>%
  dplyr::arrange(ID,Delay)
  

# 参加者ごとに描画
g_3 <- ggplot(data = theta,
       mapping = aes(x = Delay, y = ProbChooseLater)) +
  geom_hline(yintercept = 0.5, linetype = 2, colour = "gray70") +
  geom_line(aes(y = IndividualEAP), linetype = 2) +
  geom_line(size = 0.5, colour = "grey50") +
  geom_point(aes(y = IndividualEAP), shape = 2) +
  geom_point(size = 0.5, colour = "grey50") +
  facet_wrap(~ID) +
  theme_bw(base_size = 14, base_family = "HiraKakuProN-W3") + # Windowsの場合は、WindowsFonts()で確認して設定
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "デートまでの日数", y = "遅延選択肢の選択比率と選択確率(個人ごと)") +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"))

# 出力
print(g_3)



# 遅延選択肢の選択率(全体平均)
g_4 <- ggplot(data = theta,
              mapping = aes(x = Delay, y = WholeProbChooseLater)) +
  geom_hline(yintercept = 0.5, linetype = 2, colour = "gray70") +
  geom_line() +
  geom_line(aes(y = WholeEAP), linetype = 2) +
  geom_point() +
  geom_point(aes(y = WholeEAP), shape = 2) +
  theme_bw(base_size = 14, base_family = "HiraKakuProN-W3") + # Windowsの場合は、WindowsFonts()で確認して設定
  labs(x = "デートまでの日数", y = "遅延選択肢の選択比率と選択確率(全体)")

# 描画
print(g_4)

# ならべて描画
Rmisc::multiplot(g_3, g_4, cols = 2)

# 保存
ggsave(Rmisc::multiplot(g_3, g_4, cols = 2),
       filename = "figures/figure4.png",
       dpi = 600,
       w = 9,
       h = 5.5)


#
# 後日デートの主観的魅力(V)の可視化(本文中にはない)
#

# 個人ごとに女性モデル評定値を計算しておく
meanV <- d %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise(MeanV = mean(A)) %>% 
  ungroup() %>% 
  dplyr::distinct(MeanV) %>% 
  data.frame()

# Vのデータ取り出しと整形
V <- samples1 %>%
  dplyr::filter(str_detect(Parameter, "V")) %>%
  dplyr::group_by(Parameter) %>%
  dplyr::summarise(EAP = mean(value)) %>%
  ungroup() %>%
  cbind(d,.) %>%
  dplyr::group_by(ID, Delay) %>%
  dplyr::mutate(MeanV = mean(A),
                IndividualEAP = mean(EAP)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Delay) %>%
  dplyr::mutate(WholeMeanV = mean(A),
                WholeEAP = mean(EAP)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-C, -Parameter, -U, -A, -EAP) %>%
  rbind(data.frame(ID = seq(1:max(d$ID)),
                   Delay = rep(0),
                   MeanV = meanV$MeanV,
                   IndividualEAP = meanV$MeanV,
                   WholeMeanV = rep(mean(d$U)),
                   WholeEAP = rep(mean(d$U)))) %>%
  dplyr::arrange(ID,Delay)


# 可視化
g_5 <- ggplot(data = V,
              mapping = aes(x = Delay, y = IndividualEAP)) +
  geom_hline(aes(yintercept = MeanV), linetype = 2) +
  geom_point(shape = 21) +
  geom_line() +
  facet_wrap(~ID) +
  theme_linedraw(base_size = 14, base_family = "HiraKakuProN-W3") + # Windowsの場合は、WindowsFonts()で確認して設定
  scale_y_continuous(limits = c(0,110)) +
  labs(x = "デートまでの日数", y = "デートの魅力推定値(個人ごと)")

# 出力
print(g_5)


# 遅延選択肢の選択率(全体平均)
g_6 <- ggplot(data = V,
              mapping = aes(x = Delay, y = WholeEAP)) +
  geom_hline(yintercept = mean(d$A), linetype = 2) +
  geom_point(aes(y = WholeEAP), shape = 21) +
  geom_line() +
  theme_linedraw(base_size = 14, base_family = "HiraKakuProN-W3") + # Windowsの場合は、WindowsFonts()で確認して設定
  labs(x = "デートまでの日数", y = "デートの魅力推定値(全体)")

# 出力
print(g_6)



# ならべて描画
Rmisc::multiplot(g_5, g_6, cols = 2)


# 集計値
# 全体
V %>%
  dplyr::group_by(Delay) %>%
  dplyr::summarise(meanV = mean(IndividualEAP)) %>%
  formattable()


# 個人ごと
V %>%
  dplyr::group_by(ID,Delay) %>%
  dplyr::summarise(meanV = mean(IndividualEAP)) %>%
  formattable()


# モデル2 総和型わくわく割引モデル------------------------------------------

# Stanモデルの読み込みとコンパイル
model2 <- stan_model("models/model_2.stan")
# saveRDS(model2, file = "models/model_2.rds") # コンパイル済みモデルを保存する場合
# model2 <- readRDS("models/model_2.rds") # コンパイル済みモデルを読み込む場合

# 推定の実行
fit2 <- sampling(model2,
                 data = datastan,
                 seed = 123,
                 iter = 4000,
                 warmup = 2000,
                 thin = 4,
                 control = list(adapt_delta = 0.9)
                 )

# 結果の保存
# save(fit2, file = "results/fit2.Rdata", compress = "xz")
# load("results/fit2.Rdata") # 結果を読み込む場合

# Gelman-Rubin検定 DIAGNOSEから確認する
# launch_shinystan(fit2)

# WAICとLOOCV
waic(loo::extract_log_lik(fit2))
loo(loo::extract_log_lik(fit2))


#
# サンプルの取り出しと可視化
#

# サンプルの取り出し
samples2 <- ggmcmc::ggs(fit2)

# 選択確率thetaの整理
theta2 <- samples2 %>%
  dplyr::filter(str_detect(Parameter, "theta")) %>%
  dplyr::group_by(Parameter) %>%
  dplyr::summarise(EAP = mean(value)) %>%
  ungroup() %>%
  cbind(d,.) %>%
  dplyr::group_by(ID, Delay) %>%
  dplyr::mutate(ProbChooseLater = mean(C),
                IndividualEAP = mean(EAP)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Delay) %>%
  dplyr::mutate(WholeProbChooseLater = mean(C),
                WholeEAP = mean(EAP)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-C, -Parameter, -U, -A, -EAP) %>%
  rbind(data.frame(ID = seq(1:max(d$ID)),
                   Delay = rep(0),
                   ProbChooseLater = rep(0.5),
                   IndividualEAP = rep(0.5),
                   WholeProbChooseLater = rep(0.5),
                   WholeEAP = rep(0.5))) %>%
  dplyr::arrange(ID,Delay)


# 可視化
g_7 <- ggplot(data = theta2,
              mapping = aes(x = Delay, y = ProbChooseLater)) +
  geom_hline(yintercept = 0.5, linetype = 2, colour = "gray70") +
  geom_line(size = 0.5, colour = "grey50") +
  geom_line(aes(y = IndividualEAP), linetype = 2) +
  geom_point(size = 0.5, colour = "grey50") +
  geom_point(aes(y = IndividualEAP), shape = 2) +
  facet_wrap(~ID) +
  theme_bw(base_size = 14, base_family = "HiraKakuProN-W3") + # Windowsの場合は、WindowsFonts()で確認して設定
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "デートまでの日数", y = "遅延選択肢の選択比率と選択確率(個人ごと)") +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"))

# 出力
print(g_7)



# 遅延選択肢の選択率(全体平均)
g_8 <- ggplot(data = theta2,
              mapping = aes(x = Delay, y = WholeProbChooseLater)) +
  geom_hline(yintercept = 0.5, linetype = 2, colour = "gray70") +
  geom_point() +
  geom_point(aes(y = WholeEAP), shape = 2) +
  geom_line() +
  geom_line(aes(y = WholeEAP), linetype = 2) +
  theme_bw(base_size = 14, base_family = "HiraKakuProN-W3") + # Windowsの場合は、WindowsFonts()で確認して設定
  labs(x = "デートまでの日数", y = "遅延選択肢の選択比率と選択確率(全体)")

# 出力
print(g_8)


# ならべて描画
Rmisc::multiplot(g_7, g_8, cols = 2)

# 保存
ggsave(Rmisc::multiplot(g_7, g_8, cols = 2),
       filename = "figures/figure5.png",
       dpi = 600,
       w = 9,
       h = 5.5)


# 超パラメータの描画(本文中にはない) --------------------------------------
HyperParams <- samples2 %>%
  dplyr::filter(Parameter == "alpha_mu" | Parameter == "beta_mu" | Parameter == "gamma_d_mu" | Parameter == "gamma_s_mu") 

# αの階層レベル平均パラメータ
alpha_mu <- ggplot(data = HyperParams %>% dplyr::filter(Parameter == "alpha_mu"),
                   mapping = aes(x = value, y = ..density..)) +
  geom_histogram(colour = "black", alpha =0.7) +
  geom_density(alpha = 0.2) +
  theme_linedraw(base_size = 12, base_family = "HiraKakuProN-W3") +
  labs(title = "A", x = "推定値", y = "確率密度")

# βの階層レベル平均パラメータ
beta_mu <- ggplot(data = HyperParams %>% dplyr::filter(Parameter == "beta_mu"),
                  mapping = aes(x = value, y = ..density..)) +
  geom_histogram(colour = "black", alpha =0.7) +
  geom_density(alpha = 0.2) +
  theme_linedraw(base_size = 12, base_family = "HiraKakuProN-W3") +
  labs(title = "B", x = "推定値", y = "確率密度")

# γdの階層レベル平均パラメータ
gamma_d_mu <- ggplot(data = HyperParams %>% dplyr::filter(Parameter == "gamma_d_mu"),
                     mapping = aes(x = value, y = ..density..)) +
  geom_histogram(colour = "black", alpha =0.7) +
  geom_density(alpha = 0.2) +
  theme_linedraw(base_size = 12, base_family = "HiraKakuProN-W3") +
  labs(title = "C", x = "推定値", y = "確率密度")

# γsの階層レベル平均パラメータ
gamma_s_mu <- ggplot(data = HyperParams %>% dplyr::filter(Parameter == "gamma_s_mu"),
                     mapping = aes(x = value, y = ..density..)) +
  geom_histogram(colour = "black", alpha =0.7) +
  geom_density(alpha = 0.2) +
  theme_linedraw(base_size = 12, base_family = "HiraKakuProN-W3")+
  labs(title = "D", x = "推定値", y = "確率密度")

# ならべて描画
Rmisc::multiplot(alpha_mu, beta_mu, gamma_d_mu, gamma_s_mu, cols = 4)




# 後日デートの主観的魅力(V)の可視化 ----------------------------------------
# 個人ごとに女性モデル評定値を計算しておく 
meanV <- d %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise(MeanV = mean(A)) %>% 
  ungroup() %>% 
  dplyr::distinct(MeanV) %>% 
  data.frame()

# Vの描画
V2 <- samples2 %>%
  dplyr::filter(str_detect(Parameter, "V")) %>%
  dplyr::group_by(Parameter) %>%
  dplyr::summarise(EAP = mean(value)) %>%
  ungroup() %>%
  cbind(d,.) %>%
  dplyr::group_by(ID, Delay) %>%
  dplyr::mutate(MeanV = mean(A),
                IndividualEAP = mean(EAP)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Delay) %>%
  dplyr::mutate(WholeMeanV = mean(A),
                WholeEAP = mean(EAP)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-C, -Parameter, -U, -A, -EAP) %>%
  rbind(data.frame(ID = seq(1:max(d$ID)),
                   Delay = rep(0),
                   MeanV = meanV$MeanV,
                   IndividualEAP = meanV$MeanV,
                   WholeMeanV = rep(mean(d$U)),
                   WholeEAP = rep(mean(d$U)))) %>%
  dplyr::arrange(ID,Delay)


#可視化
g_9 <- ggplot(data = V2,
              mapping = aes(x = Delay, y = IndividualEAP)) +
  geom_hline(aes(yintercept = MeanV), linetype = 2) +
  geom_point(shape = 21) +
  geom_line() +
  facet_wrap(~ID) +
  theme_linedraw(base_size = 14, base_family = "HiraKakuProN-W3") + # Windowsの場合は、WindowsFonts()で確認して設定
  scale_y_continuous(limits = c(0,110)) +
  labs(x = "デートまでの日数", y = "デートの魅力推定値(個人ごと)")

# 出力
print(g_9)


# 遅延選択肢の価値(全体平均)
g_10 <- ggplot(data = V2,
              mapping = aes(x = Delay, y = WholeEAP)) +
  geom_hline(yintercept = mean(d$A), linetype = 2) +
  geom_point(aes(y = WholeEAP), shape = 21) +
  geom_line() +
  theme_linedraw(base_size = 14, base_family = "HiraKakuProN-W3") + # Windowsの場合は、WindowsFonts()で確認して設定
  labs(x = "デートまでの日数", y = "デートの魅力推定値(全体)")

# 出力
print(g_10)

# ならべて描画
Rmisc::multiplot(g_9, g_10, cols = 2)

# 保存
ggsave(Rmisc::multiplot(g_9, g_10, cols = 2),
       filename = "figures/figure6.png",
       dpi = 600,
       w = 9,
       h = 5.5)


# 集計値
V2 %>%
  dplyr::group_by(Delay) %>%
  dplyr::summarise(meanV = mean(IndividualEAP)) %>%
  formattable()


# Fin
