#R(version 3.4.4)
#rstan(version 2.17.3)
#Rtools34
#ggplot2(version 3.0.0)
#Windows 10(64bit)

# set up--------------------------------------------------
library(rstan)
dat_h <- read.csv("data/sample_data_h.txt", header = T)


# figure 9.1--------------------------------------------------
g <- ggplot(data = dat_h,
            mapping = aes(x = trial, y = total_score, size = bonus)) +
  theme_bw(base_size = 15) +
  geom_point() +
  ylab("total score") +
  scale_x_continuous(breaks = seq(from = 1, to = 7, by=1))

print(g)


# run model1-1-1.stan--------------------------------------------------
stanmodel_rc1 <- stan_model("model/model_1-1-1.stan")
U <- 100.0
standata_rc1 <- list(N_obs = nrow(subset(dat_h, censored == 0)),
                     Y = subset(dat_h, censored == 0)$total_score,
                     U = U
                     )

fit_rc1 <- sampling(object = stanmodel_rc1,
                    data = standata_rc1,
                    seed = 1234,
                    iter = 100000,
                    warmup = 50000,
                    thin = 10,
                    init = function(){list(mu = runif(1, 80, 120))}
                    )

stan_trace(fit_rc1)
stan_ac(fit_rc1)
stan_dens(fit_rc1, separate_chains = TRUE)
print(fit_rc1, digits = 4)
