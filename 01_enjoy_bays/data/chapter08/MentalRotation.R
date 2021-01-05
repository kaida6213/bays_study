#Stan2.16, rstan2.16.2, Rtools3.4, R3.4.2, Windows10�œ���m�F�ς�
library(rstan)
library(HDInterval) #�ō����x��� (HDI) �̌v�Z�Ɏg�p

##01. �֐��̒�`##
#MAP��Ԃ��֐�
map <- function(z) density(z)$x[which.max(density(z)$y)]
#�����v���Z�X���f���ɂ�锽�����Ԃ̗\���l��Ԃ��֐�
fun.mpmodel <- function(Angle,base,rate,expo) base + rate * Angle * (Angle/180)^expo

##02. �f�[�^�̓ǂݍ���##
d_raw <- read.csv("data.csv")
#head(d_raw) #�f�[�^�̊m�F
#nrow(d_raw) #�S���s��

##03. �s�������s�̏��O##
d_cor <- d_raw[d_raw$Err == 0,]
#sum(d_raw$Err == 1) #�s�������s�̐�

##04. �͂���l���s�̏��O##
d <- d_cor[(d_cor$RT > 200 & d_cor$RT < 5000),] #�L��RT:200-5000ms
#sum(d$RT<200) #200ms�����̎��s��
#sum(d$RT>5000) #5000ms���߂̎��s��

##05. Stan��MCMC�̎��s##
NP <- length(unique(d$ID))
stanmodel <- stan_model(file="MentalRotation.stan")
data <- list(N = nrow(d), NP = NP, Angle = d$RotAngle, RT = d$RT, Par = d$ID)

fit <- sampling(stanmodel,data=data,seed=999,
                chains=4,iter=4500,warmup=500,thin=1,
                include=FALSE, pars=c("m","s","local")
)

#save.image("MCMC.RData") #���ʂ̕ۑ�
#load("MCMC.Rdata") #���ʂ̓ǂݍ���

S <- summary(fit)$summary
E <- rstan::extract(fit)

##06. ���茋�ʂ���������m�F
head(S, n = 10) #summary��10�s�������\��
S[,c("n_eff","Rhat")] #n_eff��Rhat�̊m�F
max(S[,"Rhat"], na.rm=TRUE) #Rhat�̍ő�l

##07. �O���[�o���p�����[�^�̗v��
d_E <- data.frame(
  #����
  m_base = E$m_base,
  m_rate = E$m_rate,
  m_expo = E$m_expo,
  m_sigma = E$m_sigma,
  m_tau = 1/E$m_lambda,
  
  #�W���΍�
  s_base = E$s_base,
  s_rate = E$s_rate,
  s_log_expo = E$s_log_expo,
  s_sigma = E$s_sigma,
  s_log_lambda = E$s_log_lambda,
  
  #�p�����[�^�Ԃ̑��֌W��
  r_base_rate = E$rho[,1,2],
  r_base_expo = E$rho[,1,3],
  r_base_sigma = E$rho[,1,4],
  r_base_lambda = E$rho[,1,5],
  r_rate_expo = E$rho[,2,3],
  r_rate_sigma = E$rho[,2,4],
  r_rate_lambda = E$rho[,2,5],
  r_expo_sigma = E$rho[,3,4],
  r_expo_lambda = E$rho[,3,5],
  r_sigma_lambda = E$rho[,4,5]
)

summary_global <- data.frame(
  MAP = apply(d_E,2,map),
  HDI95_L = apply(d_E,2,hdi)[1,],
  HDI95_U = apply(d_E,2,hdi)[2,]
)

rownames(summary_global) <- colnames(d_E)

round(summary_global, digits=2) #MAP��95% HDI�̐��茋�� (�\8.1)

##08. ���[�J���p�����[�^��MAP���v�Z
d_line <- data.frame(ID = 0,Angle=0,RT_mu = 0,RT=0,L95=0,U95=0,L50=0,U50=0)
summary_local <- list()
MAP_local <- data.frame(base=1:NP,rate=1:NP,expo=1:NP,sigma=1:NP,tau=1:NP)

for (i in 1:NP){
  di_E <- data.frame(
    base = E$base[,i],
    rate = E$rate[,i],
    expo = E$expo[,i],
    sigma = E$sigma[,i],
    tau = 1/E$lambda[,i],
    RT_pred = E$RT_pred[,i]
  )
  
  di_est <- data.frame(
    MAP = apply(di_E,2,map),
    HDI95_L = apply(di_E,2,hdi)[1,],
    HDI95_U = apply(di_E,2,hdi)[2,],
    HDI50_L = apply(di_E,2,function(x) hdi(x,credMass = 0.50))[1,],
    HDI50_U = apply(di_E,2,function(x) hdi(x,credMass = 0.50))[2,]
  )
  
  di_line <- data.frame(ID=rep(i,361),Angle=0:360,RT_mu = fun.mpmodel(c(0:180,179:0),di_est$MAP[1],di_est$MAP[2],di_est$MAP[3]))
  di_line$RT <- di_line$RT_mu +map(di_E$RT_pred)
  di_line$L95 <- di_line$RT_mu + di_est$HDI95_L[6]
  di_line$U95 <- di_line$RT_mu + di_est$HDI95_U[6]
  di_line$L50 <- di_line$RT_mu + di_est$HDI50_L[6]
  di_line$U50 <- di_line$RT_mu + di_est$HDI50_U[6]
  
  d_line <- rbind(d_line,di_line)
  summary_local[[i]] <- di_est[1:5,]
  MAP_local[i,] <- di_est[1:5,"MAP"]
}

d_line <- d_line[-1,]

summary_local #�����Q���҂��Ƃ̃p�����[�^��MAP��HDI
MAP_local #���[�J���p�����[�^��MAP (�\8.2)

##09. ����\�����z�̃O���t���쐬 (�}8.6)##
maxRT <- 2000 #�`�悷��RT�̏�����w��

di <- d[d$RT < maxRT ,]
marY <- 56

g <- ggplot(di, aes(x = Angle, y= RT))
g <- g + theme_bw() + facet_wrap(~ID, ncol=3)
g <- g + geom_ribbon(data=d_line,aes(ymin=L95,ymax=U95), alpha=0.2)
g <- g + geom_ribbon(data=d_line,aes(ymin=L50,ymax=U50), alpha=0.5)
g <- g + geom_point(alpha=0.5,size=0.8, shape=16, color="black")
g <- g + geom_line(data=d_line,aes(x=Angle,y=RT),size=1)
g <- g + xlab("�����̌��� (���v���)") + ylab("�������� (�~���b)") 
g <- g + theme(panel.grid = element_blank(), panel.border = element_rect(size = 1))
g <- g + theme(axis.text=element_text(size=12, color="black"))
g <- g + theme(axis.title=element_text(size=14, color="black"))
g <- g + coord_cartesian(ylim=c(200+marY,maxRT-marY),xlim=c(0,360))
g <- g + scale_y_continuous(breaks = seq(200,maxRT,by = 200))
g <- g + scale_x_continuous(breaks = seq(0,360,by = 60),labels = paste(seq(0,360,by = 60),"��",sep=""))
plot(g)

#ggsave(file = "results.png", plot = g, dpi = 600, width = 9, height = 12) #�O���t�̕ۑ�
