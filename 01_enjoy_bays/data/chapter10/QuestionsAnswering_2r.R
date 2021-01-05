
rm(list=ls())

gc(T)
memory.limit(size=8118*5)

# JAGSを別途下記よりダウンロード・インストールしておく
# http://mcmc-jags.sourceforge.net/
library(rjags)
library(mcmcplots)
library(loo)

set.seed(1)

# データの読み込み
# データは http://dx.doi.org/10.7910/DVN/SD7SVE より入手してカレントディレクトリに置いておく
load("sapaTempData696items08dec2013thru26jul2014.RData")
data <- sapaTempData696items08dec2013thru26jul2014

# パーソナリティ項目をとる
varnames <- colnames(data)
indx.q <- grep("q_",varnames)
qdata <- data[,indx.q]

# 回答項目数
q.notmiss <- 1-is.na(qdata)
n.answered <- rowSums(q.notmiss)

# パーソナリティ項目に1つも答えていない1人の回答者を除く
yy <- n.answered
id0 <- which(yy==0)
yy <- yy[-id0]

n.burnin <- 1000
n.mcmc <- 4000

nPeople <- length(yy)

list.data <- list(
  nPeople = nPeople,
  y = yy,
  b = 1:14
)

# alphaの初期値を与えるための関数
initalpha <- function(x){ 
  res <- rep(NA,length(x))
  for (i in 1:length(x)){
    if (x[i] <= 72){
      res[i] <- max(ceiling(x[i]/18),1)
    } else {
      res[i] <- ceiling((x[i]-72)/25 + 4)
    }
  }
  return(res)
}

# JAGSの初期値と乱数の種の設定

list.inits <- vector("list",3)
for (i in 1:3){
  list.inits[[i]] <- list(
    theta = 0.5,
    alphTmp = initalpha(yy), 
    psi = 0.95,
    omega = 0,
    .RNG.name = "base::Mersenne-Twister",
    .RNG.seed = i
  )
}

# JAGSを用いた推定

jagsmodel <- jags.model(
  file = "QuestionsAnswering_2s.txt",
  data = list.data,
  inits = list.inits, 
  n.chain = 3,
  n.adapt=n.burnin
)

post.list <- coda.samples( jagsmodel, variable.names=c("theta","pp","predy","alpha"),n.iter=n.mcmc)

# 事後予測分布のとりだし

varnames <- colnames(post.list[[1]])
indx.predy <- grep("predy",varnames)
predy.post.list <- vector("list",3)
for (i in 1:3){
  predy.post.list[[i]] <- post.list[[i]][,indx.predy]
}
predy.post <- unlist(predy.post.list)

h<-hist(predy.post,breaks=0:322,xlim=c(0,322))

hist(yy,col="gray62",breaks=0:322,xlim=c(0,322))

xhist<-c(min(h$breaks),h$breaks)
yhist<-c(0,h$density,0)

# 事後予測分布のプロット

png("PersonalityPPredictModel2.png", height=500, width=800, pointsize=19) 
par(mai=c(1.1, 1.1, 0.05, 0.05))

hist(yy,col="gray51",border="gray51",breaks=0:322,xlim=c(0,322),cex.lab=1.23,xlab="回答項目数",ylab="度数",main="",mgp = c(2.2, 1, 0))
par(new=T)
plot(xhist,yhist,type="s",col="black",xlim=c(0,322),xlab="",ylab="",yaxt="n")

dev.off()

