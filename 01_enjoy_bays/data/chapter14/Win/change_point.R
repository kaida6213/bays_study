# 執筆量モニタリングにおける変化点検出

# 使用するRパッケージ
# 本Rコードでは，rstanパッケージ以外にデータハンドリングと図の作成のために，
# tidyverseパッケージとgridExtraパッケージを使います。
# インストールされていない方は以下の2行を，#を外して実行してインストールください。
# install.packages("tidyverse")
# install.packages("gridExtra")
library(tidyverse)
library(gridExtra)
library(rstan)

# データの読み込み
data <- read_csv("data.csv")　　　　　　　#７つの原稿の123日分の執筆状況データ
draft_info <- read_csv("draft_info.csv")　#７つの原稿の情報（合計文字数と動機づけの有無）

# 各原稿の特徴の確認（書籍の表１）
draft_info

# 原稿の合計文字数の要約統計量
summary(draft_info$total)

# 各原稿の累積執筆量のプロット（１つの図に重ねて表示）
data %>% 
  gather(draft,value,-day) %>% 
  ggplot(aes(x = day, y = value,color = draft)) +
  geom_line()  +
  geom_point(size = 0.4) +
  scale_y_continuous(name = "Writing amount (characters)") +
  scale_x_continuous(name = "Day")

# 各原稿の累積執筆量のプロット（別々の図を並べて表示：書籍の図2）
# 各原稿の累積執筆量をプロットし，d1?d7に格納
for(i in 1:7 ){
  eval(parse(text = paste0("d",i, "<- data %>%",
                           "ggplot(aes(x = day, y = draft_",i,")) +
                                geom_point(size = 0.2) ")))}
# d1～d7を並べてプロット
grid.arrange(d1,d2,d3,d4,d5,d6,d7,ncol = 2)
# うまく表示されない場合は，再度，上のコードを実行ください。

# 累積執筆量のシミュレーション（書籍の図3）
mu_1 <- 50   #変化点前のその日の執筆量の平均
mu_2 <- 400  #変化点後前のその日の執筆量の平均
sigma <- 30　#標準偏差
cp <- 23     #変化点
day <- 1:31  #執筆日数
text <- NULL #累積執筆量を格納する場所
for(i in 1:length(day)){　# 生成モデルに従って累積執筆量を作成
  if(i <= cp){
    if(i==1){
      text[1] <- rnorm(1,mu_1,sigma)
    }else{
      text[i] <- rnorm(1,mu_1 + text[i-1],sigma)
    }
  }else{
    text[i] <- rnorm(1,mu_2 + text[i-1],sigma)
  }
}
sim_data <- tibble(day = day,text = text) #データフレーム化
sim_data %>%　　# 図3の作成
  ggplot(aes(x = day, y = text)) +
  geom_point(shape = 1,size = 1) +
  geom_line(aes(x = day, y = text)) +
  geom_vline(xintercept=cp, linetype="dashed", colour="black") 

# 変化点検出のStanコードをコンパイル
change_point_c <- stan_model(file = 'change_point.stan')

# MCMCの実行と推定結果を出力する自作関数writing_analysis()の読み込み
source("writing_analysis_function.R")

# writing_analysis()関数の使い方
# writing_analysis()関数では，日数の時系列データと文章量の時系列データの２つ引数がある。
# writing_analysis(日数の時系列データ,文章量の時系列データ)と設定して実行すると
# StanによるMCMCサンプリングと結果と図の出力をする。
# 出力は以下の通りになる。
# 　change_point: 変化点検出モデルのMCMCサンプル
# 　change_point_Rhat: 変化点検出モデルのRhat
# 　cp: 変化点
# 　cp_plot: 変化点の確率分布
# 　data_cp_plot: データに変化点をプロット

# writing_analysis()関数を用いた各原稿の解析
# なおdraft_6は,効率的なサンプリングができておらず推定に
# 時間がかかるので,スキップしてもよいかもしれない
draft_1 <- writing_analysis(data$day,data$draft_1)
draft_2 <- writing_analysis(data$day,data$draft_2)
draft_3 <- writing_analysis(data$day,data$draft_3)
draft_4 <- writing_analysis(data$day,data$draft_4)
draft_5 <- writing_analysis(data$day,data$draft_5)
draft_6 <- writing_analysis(data$day,data$draft_6) 
draft_7 <- writing_analysis(data$day,data$draft_7)

# 収束や推定値の確認(draft_1の部分を変更すれば他も確認できる)
options(max.print = 10000)
draft_1$change_point
draft_1$change_point_Rhat
draft_1$cp
draft_1$cp_plot
draft_1$data_cp_plot

# cpと興味・執筆分量との関連
draft_info$cp <- c(draft_1$cp,draft_2$cp,draft_3$cp,draft_4$cp,
                   draft_5$cp,draft_6$cp,draft_7$cp)
draft_info$cp_before_deadline <- 123 - draft_info$cp
draft_info　#書籍の表2

# 執筆量データに変化点を追加してプロット（別々の図を並べて表示：書籍の図３）
grid.arrange(draft_1$data_cp_plot,draft_2$data_cp_plot,draft_3$data_cp_plot,
             draft_4$data_cp_plot,draft_5$data_cp_plot,draft_6$data_cp_plot,
             draft_7$data_cp_plot,ncol = 2)

# 生成量によって生成したcpの相対度数
cp_relative_freq <- NULL
for(j in 1:7){
  # 生成量から各cpの度数を抽出し，相対度数を計算してdraft_infoに追加&プロット
  eval(parse(text = paste0("cp_table <- as.data.frame(table(extract(draft_",j,"$change_point)$cp_s))")))
  cp_table$Freq <- cp_table$Freq/sum(cp_table$Freq) 
  cp_relative_freq[j] <- cp_table$Freq[which(cp_table$Var1 == draft_info$cp[j])]
  eval(parse(text = paste0("cp_rel_freq_draft_",j,"<- cp_table")))
}
draft_info$cp_relative_freq <- cp_relative_freq
#書籍の表3
draft_info 

#書籍の図5（番号を変えれば，他の原稿も確認できる）
cp_rel_freq_draft_6 %>% 　 
  ggplot(aes(x =Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "day", breaks = c("1", "20","40","60","80","100","123")) +
  ylim(c(0,1))

# 変化点と動機づけとの関係をプロット（書籍の図6左）
draft_info %>% 
  ggplot(aes(x = motivation,y = cp)) +
  geom_point(size = 3)

# 変化点と合計文字数との関係をプロット（書籍の図6右）
draft_info %>% 
  ggplot(aes(x = total,y = cp)) +
  geom_point(size = 3)