# ���M�ʃ��j�^�����O�ɂ�����ω��_���o

# �g�p����R�p�b�P�[�W
# �{R�R�[�h�ł́Crstan�p�b�P�[�W�ȊO�Ƀf�[�^�n���h�����O�Ɛ}�̍쐬�̂��߂ɁC
# tidyverse�p�b�P�[�W��gridExtra�p�b�P�[�W���g���܂��B
# �C���X�g�[������Ă��Ȃ����͈ȉ���2�s���C#���O���Ď��s���ăC���X�g�[�����������B
# install.packages("tidyverse")
# install.packages("gridExtra")
library(tidyverse)
library(gridExtra)
library(rstan)

# �f�[�^�̓ǂݍ���
data <- read_csv("data.csv")�@�@�@�@�@�@�@#�V�̌��e��123�����̎��M�󋵃f�[�^
draft_info <- read_csv("draft_info.csv")�@#�V�̌��e�̏��i���v�������Ɠ��@�Â��̗L���j

# �e���e�̓����̊m�F�i���Ђ̕\�P�j
draft_info

# ���e�̍��v�������̗v�񓝌v��
summary(draft_info$total)

# �e���e�̗ݐώ��M�ʂ̃v���b�g�i�P�̐}�ɏd�˂ĕ\���j
data %>% 
  gather(draft,value,-day) %>% 
  ggplot(aes(x = day, y = value,color = draft)) +
  geom_line()  +
  geom_point(size = 0.4) +
  scale_y_continuous(name = "Writing amount (characters)") +
  scale_x_continuous(name = "Day")

# �e���e�̗ݐώ��M�ʂ̃v���b�g�i�ʁX�̐}����ׂĕ\���F���Ђ̐}2�j
# �e���e�̗ݐώ��M�ʂ��v���b�g���Cd1?d7�Ɋi�[
for(i in 1:7 ){
  eval(parse(text = paste0("d",i, "<- data %>%",
                           "ggplot(aes(x = day, y = draft_",i,")) +
                                geom_point(size = 0.2) ")))}
# d1�`d7����ׂăv���b�g
grid.arrange(d1,d2,d3,d4,d5,d6,d7,ncol = 2)
# ���܂��\������Ȃ��ꍇ�́C�ēx�C��̃R�[�h�����s���������B

# �ݐώ��M�ʂ̃V�~�����[�V�����i���Ђ̐}3�j
mu_1 <- 50   #�ω��_�O�̂��̓��̎��M�ʂ̕���
mu_2 <- 400  #�ω��_��O�̂��̓��̎��M�ʂ̕���
sigma <- 30�@#�W���΍�
cp <- 23     #�ω��_
day <- 1:31  #���M����
text <- NULL #�ݐώ��M�ʂ��i�[����ꏊ
for(i in 1:length(day)){�@# �������f���ɏ]���ėݐώ��M�ʂ��쐬
  if(i <= cp){
    if(i==1){
      text[1] <- rnorm(1,mu_1,sigma)
    }else{
      text[i] <- rnorm(1,mu_1 + text[i-1],sigma)
    }
  }else{
    text[i] <- rnorm(1,mu_2 + text[i-1],sigma)
  }
}
sim_data <- tibble(day = day,text = text) #�f�[�^�t���[����
sim_data %>%�@�@# �}3�̍쐬
  ggplot(aes(x = day, y = text)) +
  geom_point(shape = 1,size = 1) +
  geom_line(aes(x = day, y = text)) +
  geom_vline(xintercept=cp, linetype="dashed", colour="black") 

# �ω��_���o��Stan�R�[�h���R���p�C��
change_point_c <- stan_model(file = 'change_point.stan')

# MCMC�̎��s�Ɛ��茋�ʂ��o�͂��鎩��֐�writing_analysis()�̓ǂݍ���
source("writing_analysis_function.R")

# writing_analysis()�֐��̎g����
# writing_analysis()�֐��ł́C�����̎��n��f�[�^�ƕ��͗ʂ̎��n��f�[�^�̂Q����������B
# writing_analysis(�����̎��n��f�[�^,���͗ʂ̎��n��f�[�^)�Ɛݒ肵�Ď��s�����
# Stan�ɂ��MCMC�T���v�����O�ƌ��ʂƐ}�̏o�͂�����B
# �o�͈͂ȉ��̒ʂ�ɂȂ�B
# �@change_point: �ω��_���o���f����MCMC�T���v��
# �@change_point_Rhat: �ω��_���o���f����Rhat
# �@cp: �ω��_
# �@cp_plot: �ω��_�̊m�����z
# �@data_cp_plot: �f�[�^�ɕω��_���v���b�g

# writing_analysis()�֐���p�����e���e�̉��
# �Ȃ�draft_6��,�����I�ȃT���v�����O���ł��Ă��炸�����
# ���Ԃ�������̂�,�X�L�b�v���Ă��悢��������Ȃ�
draft_1 <- writing_analysis(data$day,data$draft_1)
draft_2 <- writing_analysis(data$day,data$draft_2)
draft_3 <- writing_analysis(data$day,data$draft_3)
draft_4 <- writing_analysis(data$day,data$draft_4)
draft_5 <- writing_analysis(data$day,data$draft_5)
draft_6 <- writing_analysis(data$day,data$draft_6) 
draft_7 <- writing_analysis(data$day,data$draft_7)

# �����␄��l�̊m�F(draft_1�̕�����ύX����Α����m�F�ł���)
options(max.print = 10000)
draft_1$change_point
draft_1$change_point_Rhat
draft_1$cp
draft_1$cp_plot
draft_1$data_cp_plot

# cp�Ƌ����E���M���ʂƂ̊֘A
draft_info$cp <- c(draft_1$cp,draft_2$cp,draft_3$cp,draft_4$cp,
                   draft_5$cp,draft_6$cp,draft_7$cp)
draft_info$cp_before_deadline <- 123 - draft_info$cp
draft_info�@#���Ђ̕\2

# ���M�ʃf�[�^�ɕω��_��ǉ����ăv���b�g�i�ʁX�̐}����ׂĕ\���F���Ђ̐}�R�j
grid.arrange(draft_1$data_cp_plot,draft_2$data_cp_plot,draft_3$data_cp_plot,
             draft_4$data_cp_plot,draft_5$data_cp_plot,draft_6$data_cp_plot,
             draft_7$data_cp_plot,ncol = 2)

# �����ʂɂ���Đ�������cp�̑��Γx��
cp_relative_freq <- NULL
for(j in 1:7){
  # �����ʂ���ecp�̓x���𒊏o���C���Γx�����v�Z����draft_info�ɒǉ�&�v���b�g
  eval(parse(text = paste0("cp_table <- as.data.frame(table(extract(draft_",j,"$change_point)$cp_s))")))
  cp_table$Freq <- cp_table$Freq/sum(cp_table$Freq) 
  cp_relative_freq[j] <- cp_table$Freq[which(cp_table$Var1 == draft_info$cp[j])]
  eval(parse(text = paste0("cp_rel_freq_draft_",j,"<- cp_table")))
}
draft_info$cp_relative_freq <- cp_relative_freq
#���Ђ̕\3
draft_info 

#���Ђ̐}5�i�ԍ���ς���΁C���̌��e���m�F�ł���j
cp_rel_freq_draft_6 %>% �@ 
  ggplot(aes(x =Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "day", breaks = c("1", "20","40","60","80","100","123")) +
  ylim(c(0,1))

# �ω��_�Ɠ��@�Â��Ƃ̊֌W���v���b�g�i���Ђ̐}6���j
draft_info %>% 
  ggplot(aes(x = motivation,y = cp)) +
  geom_point(size = 3)

# �ω��_�ƍ��v�������Ƃ̊֌W���v���b�g�i���Ђ̐}6�E�j
draft_info %>% 
  ggplot(aes(x = total,y = cp)) +
  geom_point(size = 3)