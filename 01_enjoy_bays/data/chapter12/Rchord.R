library(dplyr)
library(tidyverse)
library(brms)
library(TreeBUGS)

#MAP function map_mcmc()
map_mcmc <- function(z){ density(z)$x[which.max(density(z)$y)] } 


Tai <- read.csv("~/Chapter12難波/Tai.csv",header=T)
HFT <- read.csv("~/Chapter12難波/sdtTai.csv",header=T)

#Data wandarling
sdtTai <- Tai %>% 
  mutate(type = "hit",
         type = ifelse(Feel==1 & eva==0, "miss", type),
         type = ifelse(Feel==0 & eva==0, "cr", type),  # Correct rejection
         type = ifelse(Feel==0 & eva==1, "fa", type))  # False alarm
# Count hits/misses/etc. and format data to one row per person + Snodgrass補正あり
sdtTai <- sdtTai %>% 
  group_by(obs, type) %>% 
  summarise(count = n()+0.5) %>% 
  spread(type, count)  # Format data to one row per person
sdtTai[is.na(sdtTai)]<-0.5 # NA -> 0
# Calculate point estimates of EVSDT parameters 
sdtTai <- sdtTai %>% 
  mutate(zhr = qnorm(hit / (hit+miss)),
         zfa = qnorm(fa / (fa+cr)),
         dprime = zhr-zfa,
         crit = -zfa)
sdtTai

sdtTai_sum <- select(sdtTai, obs, dprime, crit) %>%  # Select these variables only
  gather(parameter, value, -obs) %>%  # Convert data to long format
  group_by(parameter) %>%  # Prepare to summarise on these grouping variables
  # Calculate summary statistics for grouping variables
  summarise(n=n(), mu=mean(value), sd=sd(value), se=sd/sqrt(n))
sdtTai_sum


# Signal detection theory
# brm(回帰式, 誤差構造の設定, データ)
# 回帰式中の(1|ID)は切片の,(Feel|ID)は傾きの変量効果を指定。
fitF <- brm(eva ~ 1 + Feel + (1 + Feel | obs), 
              family = bernoulli(link="probit"), data = Tai)

summary(fitF)

# calculation of response bias
library(rstan)
SDTmodel <- stan_model("~/Chapter12難波/SDT.stan")
SData <- standata(fitF)
fitRB <- sampling(SDTmodel, data=SData)
summary(fitRB, pars = 'RB')


#MPT
Pert <- read.csv("~/Chapter12難波/THetPerm.csv",header=T)
tree <- list(old = c("hit","miss"),
             new = c("fa", "cr"))
tmp <- testHetPerm(Pert, tree, rep=10000, nCPU=1)
tmp[2:3]

fitH1MPT <- betaMPT(
  eqnfile="~/Chapter12難波/1HTM.eqn",         # .eqn file
  data="~/Chapter12難波/T1Tai.csv",        # individual data
  
  ### optional MCMC input:            
  n.iter = 20000,             # number of iterations
  n.burnin = 5000,            # number of burnin samples that are removed
  n.thin = 5,                 # thinning rate of removing samples
  n.chains = 3                # number of MCMC chains (run in parallel)
)

summary(fitH1MPT)

fitH2MPT <- betaMPT(
  eqnfile="~/Chapter12難波/2HTM.eqn",         # .eqn file
  data="~/Chapter12難波/T1Tai.csv",           # data
  restrictions=list("d = r"), # パラメータの制約
  
  ### optional MCMC input:   
  n.iter = 20000,             # iterationsの数
  n.burnin = 5000,            # 取り除くバーンインサンプルの数
  n.thin = 5,                 # thinning rate
  n.chains = 3                # MCMC chainsの数
)
# Default: Traceplot and density
plot(fitH2MPT,    # fitted model
     parameter ="mean"      # which parameter to plot
)
# further arguments are passed to ?plot.mcmc.list

# Auto-correlation plots:
plot(fitH2MPT,  parameter = "mean", type = "acf")

# Gelman-Rubin plots:
plot(fitH2MPT,  parameter = "mean", type = "gelman")
summary(fitH2MPT)


#model fitting, p > 0.5 is sufficient fit.
PPP(fitH1MPT)
PPP(fitH2MPT)
