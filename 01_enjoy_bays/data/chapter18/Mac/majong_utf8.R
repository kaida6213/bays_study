# Libraryの読み込み
library(tidyverse)
library(rstan)
library(reshape2)
library(pander)
library(GGally)
library(ggthemes)

# 並列化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# データの読み込み
pointData <- read.csv("pointData.csv", header = T)

# 前処理
playerNum <- 4 # プレイヤー数
hantyanNum <- nrow(pointData) # 半荘(1ゲームの単位)の数

# 麻雀のデータ(ポイント)はソフトマックス関数で変換するには値が大きすぎるので、スケールを調整する
pointData2 <- as.matrix(pointData/300)
point <- matrix(0, hantyanNum, playerNum)
for(h in 1:hantyanNum){
  # ソフトマックス関数を使って、各半荘の結果を[0,1]の範囲かつ総和が1になるように変換する
  point[h,] <- exp(pointData2[h,])/sum(exp(pointData2[h,]))
}

# 諸情報
iter <- 1000
thin <- 1
warmup <- iter/2
chainNum <- 4
sampleNum <- (iter-warmup ) * chainNum

data <- list(playerNum = playerNum,
             hantyanNum = hantyanNum,
             point = point)

parameters <- c("janryoku")

result <- stan("majong_utf8.stan",
                data = data,
                pars = parameters,
                seed = 123,
                iter = iter,
                chains = chainNum,
                thin = thin,
                warmup = warmup)

# 図表の作成
windowsFonts(gothic = windowsFont("MS Gothic"))

# 表1
table1 <- pointData[1:6,] %>%
  rbind(apply(.,2,sum))

# 行明・列名を整える
colnames(table1) <- c("A氏", "B氏", "C氏", "D氏")
rownames(table1) <- c("1半荘目", "2半荘目", "3半荘目",
                      "4半荘目", "5半荘目", "6半荘目", "合計")

pander(table1, justify = "right")

# 図1を作成
data.frame(pointData) %>%
  # 列名を変更
  rename("A氏"　= player_A, "B氏" = player_B, "C氏" = player_C, "D氏" = player_D) %>%
  # ロングデータに変形
  gather(key = player, value = point) %>%
  # ggplotによる描画
  ggplot(mapping = aes(x = player, y = point, group = 1)) +
  geom_jitter(colour = "dimgray", size = 2, alpha = 0.7,
              position = position_jitter(0.25)) +
  # 平均を点で追加
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  # 平均を結ぶ線を追加
  stat_summary(fun.y = mean, geom = "line", size = 1) +
  # 軸の書式設定など
  theme_set(theme_gray(base_size = 12, base_family = "gothic")) +
  theme(axis.title.x = element_text(colour = "black",
                                    family = "gothic"),
        axis.title.y = element_text(colour = "black",
                                    family = "gothic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.line = element_line(colour = "black"), 
        axis.ticks = element_blank()) +
  xlab("プレイヤー") + ylab("ポイント")

# 図2
# stan fitオブジェクトからjanryokuパラメータのサンプリング結果を取得
janryoku <- rstan::extract(result)$janryoku

janryoku %>%
  data.frame() %>%
  rename("A氏" = X1, "B氏" = X2, "C氏" = X3, "D氏" = X4) %>%
  gather(key = player, value = janryoku) %>%
  ggplot() + geom_density(aes(x = janryoku),
                          fill = "darkgray", colour = "darkgray", alpha = 0.8) +
  facet_wrap(~player) +
  theme(axis.title.x = element_text(colour = "black",
                                    family = "gothic"),
        axis.title.y = element_text(colour = "black",
                                    family = "gothic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.line = element_line(colour = "black"), 
        axis.ticks = element_blank()) +
  xlab("janryoku") + ylab("確率密度")

# 表2
# 水準間に差がある確率を算出する関数
computeProb <- function(parameter, i, j){
  # i番目のプレイヤーの雀力(abilityA or abilityB)の事後分布からの乱数から
  # j番目のプレイヤーの雀力(abilityA or abilityB)の事後分布の乱数を引き算し、
  # それが0である頻度を乱数の数で除算する 
  # = i番目のプレイヤーの雀力がj番目のプレイヤーの雀力より高い確率
  temp <- parameter[,i] - parameter[,j] > 0
  temp <- sum(temp)/sampleNum
  return(temp)
}

# 表の各行を作成
playerA = c("-", computeProb(janryoku,1,2), computeProb(janryoku,1,3), computeProb(janryoku,1,4))
playerB = c(computeProb(janryoku,2,1), "-", computeProb(janryoku,2,3), computeProb(janryoku,2,4))
playerC = c(computeProb(janryoku,3,1), computeProb(janryoku,3,2), "-",computeProb(janryoku,3,4))
playerD = c(computeProb(janryoku,4,1), computeProb(janryoku,4,2), computeProb(janryoku,4,3), "-")

# 上で作成した各行を結合、列名を設定
table2 <- rbind(playerA, playerB, playerC, playerD)
colnames(table2) <- c("A氏", "B氏", "C氏", "D氏")

# 表として出力
pander(table2, justify = "right")


# B氏がA氏、C氏、D氏よりも強い確率
(sum(janryoku[,2] - janryoku[,1] > 0) + sum(janryoku[,2] - janryoku[,3] > 0)+ sum(janryoku[,2] - janryoku[,4] > 0))/ (sampleNum*3)

# 図3
multiScatter <- janryoku %>%
  data.frame() %>%
  rename("A氏" = X1, "B氏" = X2, "C氏" = X3, "D氏" = X4) %>%
  ggpairs(upper = list(continuous = wrap(ggally_cor,
                                         colour = "black",
                                         size = 5)),
          diag = list(continuous = wrap("barDiag",
                                        color = "white",
                                        fill = "black",
                                        alpha = 0.7)),
          lower = list(continuous = wrap(ggally_points,
                                         size = 0.1,
                                         color = "darkgray"))) +
  theme_gray(base_family = "gothic") + 
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.line = element_line(colour = "black"), 
        axis.ticks = element_blank())

for (i in 2:multiScatter$nrow){
  for (j in 1:(i-1)){
    multiScatter[i,j] = multiScatter[i,j] + 
      geom_abline(intercept = 0, slope = 1,
                  colour = "black")
  }
  }
