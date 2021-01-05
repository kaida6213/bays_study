# MCMC�̎��s�Ɛ��茋�ʂ��o�͂��鎩��֐�writing_analysis()
writing_analysis <- function(day,text) {
  # �T���v�����O�̐ݒ�i�ŏI�I��5000�T���v�����擾����ݒ�ɂȂ��Ă���j
  rstan_options(auto_write=TRUE)
  options(mc.cores = parallel::detectCores())
  hmcIter = 5500
  hmcChains = 4
  hmcWarmup = 500
  hmcThin = 4
  # �ω��_�̐���i��������1000�Ŋ����ăX�P�[���𒲐����Ă���j
  change_point <- sampling(change_point_c,
                           seed = 1234,
                           data = list(n = length(text), text = text/1000),
                           warmup = hmcWarmup, 
                           iter = hmcIter,
                           chains = hmcChains,
                           thin =hmcThin) 
  # �����̊m�F�iRhat��1.10�ȉ��������j
  change_point_Rhat <- all(summary(change_point)$summary[,"Rhat"] < 1.10, na.rm = T)
  # �v���b�g�p�f�[�^�̏���
  data <- data.frame(day,text)
  # �ω��_�̊m�F
  q <- colMeans(exp(extract(change_point)$lp))
  data$p_cp <- q/sum(q)
  cp <- data$day[which.max(data$p_cp)]
  # �ω��_�̊m���̃v���b�g
  cp_plot <- data %>%
    ggplot(aes(x = day, y = p_cp)) +
    geom_line(aes(x = day, y = p_cp)) +
    scale_y_continuous(name = "Probability") 
  
  # CP�Ɨ\����Ԃ̃v���b�g
  # ���㕪�z��mu_1,mu_2,sigma�̃T���v����1000���|���Č��̒l�ɖ߂�
  mu_1_sample <- extract(change_point)$mu_1*1000
  mu_2_sample <- extract(change_point)$mu_2*1000
  sigma_sample <- extract(change_point)$sigma*1000
  med <- NULL
  up_95 <- NULL
  lo_95 <- NULL
  # �e���_�ɂ����钆���l�C95%�\����Ԃ��Z�o
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
  # ��L�ō쐬�����\����Ԃ��v���b�g
  data_cp_plot <- plot_pred %>%
    ggplot(aes(x = day, y = text)) +
    geom_point(shape = 1,size = 1) +
    geom_line(aes(x = day, y = Median)) +
    geom_ribbon(aes(ymin=lower_95, ymax=upper_95), fill='black', alpha=0.25)+
    geom_vline(xintercept=cp, linetype="dashed", colour="black") 
  
  # ���ʂ̏o��
  return(list(change_point = change_point,
              change_point_Rhat = change_point_Rhat,
              cp = cp,
              cp_plot = cp_plot,
              data_cp_plot = data_cp_plot
  )) 
}