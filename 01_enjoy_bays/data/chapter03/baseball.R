library(rstan)
library(MASS)

#function
map <- function(z){ 
  density(z)$x[which.max(density(z)$y)] 
}

##data set################################################
dat <- read.csv("baseball_team.csv")
head(dat)

#histgram
par(cex.lab=1.5,cex.axis=1)
hist(dat$T,xlab="阪神タイガース",ylab="度数")


##stan model##############################################
model <- stan_model("unfolding_baseball.stan")
saveRDS(model,"model.rds")

model <- readRDS("model.rds")

##Variational Bayes#######################################
#data
N <- nrow(dat)
P <- ncol(dat)
K <- 2
Y <- dat/100 #scaling data
R <- 3
data.vb <- list(N=N,P=P,K=K,Y=Y,R=R)

#initial value
delta_init <- matrix(c(-2,2,1,0,-1,-1,2,2,-2,-2,-2,-2),ncol=2)
init.vb <- list(delta=delta_init,alpha=1,beta=0.2)

#variational bayes
#変分ベイズは初期値をきめるために実行します。ただ、なかなかうまく解がもとまらないので
#何度も試して見る必要があります。
fit.vb <- vb(model,data=data.vb,init=init.vb,iter=15000,tol_rel_obj=0.005)
saveRDS(fit.vb,"fit_vb.rds")

#output
print(fit.vb,pars=c("alpha","beta","delta","sigma"))


##MCMC sampling##########################################
#MCMC setting
C <- 4 #chains
I <- 6000 #iteration
W <- 1000 #warmup

#data
N <- nrow(dat)
P <- ncol(dat)
D <- 2
Y <- dat/100
R <- 3
data.mcmc <- list(N=N,P=P,D=D,Y=Y,R=R)

#initial value
alpha_init <- mean(rstan::extract(fit.vb)$alpha)
beta_init <- mean(rstan::extract(fit.vb)$beta)
sigma_init <- apply(rstan::extract(fit.vb)$sigma,2,mean)
delta_init <- apply(rstan::extract(fit.vb)$delta,c(2,3),map)
theta_init <- apply(rstan::extract(fit.vb)$theta,c(2,3),map)
gamma_init <- apply(rstan::extract(fit.vb)$gamma,2,mean)
init <- list(alpha=alpha_init,beta=beta_init,delta=delta_init,sigma=sigma_init,
             theta=theta_init,gamma=gamma_init)
init.mcmc <- list()
for(c in 1:C){
  init.mcmc[c] <- list(init)
}

#sampling
fit.mcmc <- sampling(model,data=data.mcmc,init=init.mcmc,iter=I,warmup=W,chains=C,cores=C)
saveRDS(fit.mcmc,"fit_mcmc.rds")

#convergence check
rstan::stan_rhat(fit.mcmc)
rstan::stan_ac(fit.mcmc,pars="delta")
rstan::stan_trace(fit.mcmc,pars="delta")

#output
print(fit.mcmc,pars=c("alpha","beta","delta","sigma"),probs=c(0.025,0.5,0.975))

##Mapping##################################################
#object mapping
delta <- data.frame(apply(rstan::extract(fit.mcmc)$delta,c(2,3),map))
names(delta) <- c("dim1","dim2")
rownames(delta) <- c("巨人","阪神","広島","中日","DeNA","ヤクルト")
plot(delta$dim1,delta$dim2,pch="",xlim=c(-R,R),ylim=c(-R,R),xlab="Dim1",ylab="Dim2")
text(delta$dim1,delta$dim2,labels=rownames(delta),cex=1.5,font=2)

#kernel mapping mcmc
theta <- data.frame(apply(rstan::extract(fit.mcmc)$theta,c(2,3),map))
names(theta) <- c("dim1","dim2")
knl <- kde2d(theta$dim1,theta$dim2,n=50)
contour(knl,add=T,drawlabels=T)

#3D mapping
library(rgl)
persp(knl,theta=0,phi=45, expand=0.5, col=rainbow(0),xlab="dim1",ylab="dim2",zlab="pdf")

