#R(version 3.4.4)
#rstan(version 2.17.3)
#Rtools34
#ggplot2(version 3.0.0)
#Windows 10(64bit)

# set up--------------------------------------------------
library(rstan)
dat_h <- read.csv("data/sample_data_h.txt", header = T)


# run model1-1-2.stan--------------------------------------------------
stanmodel_rc2 <- stan_model("model/model_1-1-2.stan")
U <- 100.0
standata_rc2 <- list(N = nrow(dat_h),
                     N_cens = nrow(subset(dat_h, total_score >= U)),
                     Y = dat_h$total_score,
                     censored = dat_h$censored,
                     U = U
                     )

fit_rc2 <- sampling(object = stanmodel_rc2,
                    data = standata_rc2,
                    seed = 1234,
                    iter = 100000,
                    warmup = 50000,
                    thin = 10,
                    init = function(){list(mu = runif(1, 80, 120))}
                    )

stan_trace(fit_rc2)
stan_ac(fit_rc2)
stan_dens(fit_rc2, separate_chains = TRUE)
print(fit_rc2, digits = 4)
