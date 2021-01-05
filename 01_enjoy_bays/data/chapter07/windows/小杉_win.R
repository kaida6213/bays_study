# 環境をクリア
rm(list=ls())
# 必要なパッケージの読み込み
library(tidyverse)
library(ggrepel)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Bayesian INDSCAL --------------------------------------------------------

# ファイルからデータを読み込む
image.set <- read.csv("distance_set.csv",head=F)
# Bayesian INDSCAL
## 被験者の数
N <- nrow(image.set)
## 刺激の数
I <- 10
## 刺激のペアの数
P <- (10*9)/2
## データセット
Y <- image.set[1:45]
## 世代のID
Gid <- image.set$V46
## 世代の総数
G <- max(Gid)

# Stanに渡すデータセットに組み上げる
dataset <- list(N=N,I=I,P=P,Y=as.matrix(Y),Gid=Gid,G=G)
# モデルのコンパイル
model <- stan_model("b_indscal.stan")
# サンプリング
fit <- sampling(model,dataset,iter=10000,warmup=5000)
# 重みの推定値出力
print(fit,pars="w")
# 座標の推定値出力
print(fit,pars="lambda")


# 作業1:50%確信区間を伴う布置図の描画準備 --------------------------------------------------------

  ## Stanfit objectからMCMCサンプルを取り出してデータフレームに 
fit %>% rstan::extract() %>% data.frame() %>% 
  ## 座標パラメータだけ取り出す
  dplyr::select(starts_with("lambda")) %>% 
  ## ロング型データに
  tidyr::gather(key,val) %>% 
  ## 次元，対象変数を変数名から作成，不要な列の削除
  mutate(dim=str_sub(key,start=8,end=8),
         target=str_sub(key,start=10),
         key=NULL) %>% 
  ## 対象，次元でグループ化，ネスト
  dplyr::group_by(target,dim) %>% nest %>% 
  ## 次元を横に広げ，ネスト解除
  tidyr::spread(dim,data) %>% unnest() %>% 
  ## 改めて対象でグループ化
  group_by(target) %>% 
  ## 変数名をわかりやすく
  rename(X=val,Y=val1) %>% 
  # 要約統計量の算出
  dplyr::summarise_all(funs(EAP=mean,
                            lower=quantile(.,0.25),
                            upper=quantile(.,0.75))) %>% 
  ## 対象名を因子型にしラベルをつける
  mutate(target=factor(1:10,
                       labels=c("札幌",
                                "飛騨高山",
                                "舞鶴",
                                "佐世保",
                                "志摩",
                                "秋吉台",
                                "野沢",
                                "道後",
                                "由布院",
                                "宮古島"))) -> plot.df


# 作業2:雲を纏わせるプロットの準備 --------------------------------------------------------------

## MCMCサンプルの中から一部を抽出
## 抽出するサンプル数
nsamp <- 100
  ## Stanfit objectからMCMCサンプルを取り出してデータフレームに 
fit %>% rstan::extract() %>% data.frame %>% 
  ## 座標パラメータだけ取り出す
  dplyr::select(starts_with("lambda")) %>% 
  ## MCMCサンプルからサンプリング
  sample_n(.,size=nsamp) %>% 
  ## 変数名を列名から取り込む
  tibble::rownames_to_column() %>% 
  ## ロング型データに
  tidyr::gather(key,val,-rowname) %>% 
  ## 対象名を変数につける
  mutate(label=rep(rep(c("札幌",
                         "飛騨高山",
                         "舞鶴",
                         "佐世保",
                         "志摩",
                         "秋吉台",
                         "野沢",
                         "道後",
                         "由布院",
                         "宮古島"), 
                          each=nsamp*2))) %>% 
  ## 次元変数を作成
  mutate(dim=paste0("dim",str_sub(key,start=8,end=8))) %>% 
  ## 不要な変数を削除
  mutate(key=NULL) %>%
  ## 次元を横に並べる
  tidyr::spread(dim,value=val) -> cloud.df


# 描画 ----------------------------------------------------------------------

## 元になる座標は作業1の座標
ggplot(data=plot.df,aes(x=X_EAP,y=Y_EAP)) + 
  ## 対象のプロット
  geom_point(size=3) +  labs(title="",x="dim1",y="dim2")  +
  ## エラーバーをX,Y軸に
  geom_errorbar(data=plot.df,mapping=aes(ymin=Y_lower,ymax=Y_upper),width=0.1,alpha=0.5) +
  geom_errorbarh(data=plot.df,mapping=aes(xmin=X_lower,xmax=X_upper),height=0.1,alpha=0.5) +
  ## 対象名をプロット
  geom_text_repel(data=plot.df,aes(x=X_EAP,y=Y_EAP,label=target),size=5) +
  ## 雲データをプロット
  geom_point(data=cloud.df,aes(x=dim1,y=dim2,shape=label),alpha=0.5,size=4) +
  ## シェイプの種類がデフォルトを超えるのでマニュアルで指定
  scale_shape_manual(values=1:10)


# 雲に色をつける
## 元になる座標は作業1の座標
ggplot(data=plot.df,aes(x=X_EAP,y=Y_EAP)) + 
  ## 色付きの雲を纏わせる
  geom_point(data=cloud.df,aes(x=dim1,y=dim2,shape=label,color=label),alpha=0.5) + 
  ## シェイプの種類がデフォルトを超えるのでマニュアルで指定
  scale_shape_manual(values=1:10) + 
  ## 対象名をプロット
  geom_text_repel(data=plot.df,aes(x=X_EAP,y=Y_EAP,label=target),size=5) +
  ## X,Y軸に名前をつける
  labs(title="",x="dim1",y="dim2")  

