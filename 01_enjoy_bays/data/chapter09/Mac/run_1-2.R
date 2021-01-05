#R(version 3.4.4)
#rstan(version 2.17.3)
#Rtools34
#ggplot2(version 3.0.0)
#Windows 10(64bit)

# set up--------------------------------------------------
library(rstan)
dat_s <- read.csv("data/sample_data_s.txt", header = T)


# figure 9.3--------------------------------------------------
g <- ggplot(data = dat_s,
            mapping = aes(x = trial, y = total_score, size = bonus)) +
  theme_bw(base_size = 15) +
  geom_point() +
  ylab("total score") +
  scale_x_continuous(breaks = seq(from = 0, to = 30, by=5))

print(g)


# run model_1-2.stan--------------------------------------------------
stanmodel_ss <- stan_model("model/model_1-2.stan")

T_obs <- nrow(dat_s)
T_pred <- 1
fit_ss <- sampling(object = stanmodel_ss,
                   data = list(T_obs = T_obs,
                               T_pred = T_pred,
                               Y = dat_s$total_score / 100),#1/100にスケーリング
                   seed = 1234,
                   iter = 100000,
                   warmup = 50000,
                   thin = 10
                   )

print(fit_ss, digits = 4) #1/100にスケーリングされた結果なので、100倍して解釈する


# figure 9.4--------------------------------------------------
temp <- rstan::extract(fit_ss)$mu
temp <- as.data.frame(temp)
temp[, (T_obs + 1) : (T_obs + T_pred)] <- as.data.frame(rstan::extract(fit_ss)$y_pred)
temp <- as.data.frame(apply(X = temp, MARGIN = 2, FUN = function(x){quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))}))
temp <- as.data.frame(t(temp))
temp <- temp * 100 #1/100のスケーリングを元に戻す
colnames(temp) <- c("q2.5", "q25", "q50", "q75", "q97.5")
temp$trial <- 1:nrow(temp)
row.names(temp) <- NULL

g <- ggplot(mapping = aes(x = trial)) +
  theme_bw(base_size = 20) +
  geom_ribbon(data = temp, mapping = aes(ymax = q97.5, ymin = q2.5), fill = "grey", alpha = 0.5) +
  geom_ribbon(data = temp, mapping = aes(ymax = q75, ymin = q25), fill = "dimgrey", alpha = 0.5) +
  geom_line(data = temp, mapping = aes(y = q50), linetype = "dashed") +
  geom_line(data = dat_s, mapping = aes(y = total_score), size = 1) +
  geom_point(data = dat_s, mapping = aes(y = total_score), size = 3, shape = 5) +
  ylab("total score") +
  scale_x_continuous(breaks = seq(from = 0, to = 30, by=5))

print(g)
