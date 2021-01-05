# MCMCの実行と推定結果を出力する自作関数writing_analysis()
writing_analysis <- function(day,text) {
  # サンプリングの設定（最終的に5000サンプルを取得する設定になっている）
  rstan_options(auto_write=TRUE)
  options(mc.cores = parallel::detectCores())
  hmcIter = 5500
  hmcChains = 4
  hmcWarmup = 500
  hmcThin = 4
  # 変化点の推定（文字数は1000で割ってスケールを調整している）
  change_point <- sampling(change_point_c,
                           seed = 1234,
                           data = list(n = length(text), text = text/1000),
                           warmup = hmcWarmup, 
                           iter = hmcIter,
                           chains = hmcChains,
                           thin =hmcThin) 
  # 収束の確認（Rhatが1.10以下か検討）
  change_point_Rhat <- all(summary(change_point)$summary[,"Rhat"] < 1.10, na.rm = T)
  # プロット用データの準備
  data <- data.frame(day,text)
  # 変化点の確認
  q <- colMeans(exp(extract(change_point)$lp))
  data$p_cp <- q/sum(q)
  cp <- data$day[which.max(data$p_cp)]
  # 変化点の確率のプロット
  cp_plot <- data %>%
    ggplot(aes(x = day, y = p_cp)) +
    geom_line(aes(x = day, y = p_cp)) +
    scale_y_continuous(name = "Probability") 
  
  # CPと予測区間のプロット
  # 事後分布のmu_1,mu_2,sigmaのサンプルに1000を掛けて元の値に戻す
  mu_1_sample <- extract(change_point)$mu_1*1000
  mu_2_sample <- extract(change_point)$mu_2*1000
  sigma_sample <- extract(change_point)$sigma*1000
  med <- NULL
  up_95 <- NULL
  lo_95 <- NULL
  # 各時点における中央値，95%予測区間を算出
  for(i in 1:length(day)){
    if(i <= cp){
      if(i==1){
        temp <- rnorm(length(mu_1_sample),mu_1_sample,sigma_sample)
        med[1] <- median(temp)
        lo_95[1] <- quantile(temp,probs = 0.025)
        up_95[1] <- quantile(temp,probs = 0.975)
      }else{
        temp <- rnorm(length(mu_1_sample),mu_1_sample + text[i-1],sigma_sample)
        med[i] <- median(temp)
        lo_95[i] <- quantile(temp,probs = 0.025)
        up_95[i] <- quantile(temp,probs = 0.975)      
      }
    }else{
      temp <- rnorm(length(mu_2_sample),mu_2_sample + text[i-1],sigma_sample)
      med[i] <- median(temp)
      lo_95[i] <- quantile(temp,probs = 0.025)
      up_95[i] <- quantile(temp,probs = 0.975) 
    }
  }
  plot_pred <- tibble(day = day,text = text, Median = med, lower_95 = lo_95, upper_95 = up_95)
  # 上記で作成した予測区間をプロット
  data_cp_plot <- plot_pred %>%
    ggplot(aes(x = day, y = text)) +
    geom_point(shape = 1,size = 1) +
    geom_line(aes(x = day, y = Median)) +
    geom_ribbon(aes(ymin=lower_95, ymax=upper_95), fill='black', alpha=0.25)+
    geom_vline(xintercept=cp, linetype="dashed", colour="black") 
  
  # 結果の出力
  return(list(change_point = change_point,
              change_point_Rhat = change_point_Rhat,
              cp = cp,
              cp_plot = cp_plot,
              data_cp_plot = data_cp_plot
  )) 
}