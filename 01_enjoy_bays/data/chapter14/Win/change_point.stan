data {
  int n;         // �f�[�^�|�C���g��
  real text[n];  // ������
}

parameters {
  real<lower = -100, upper = 100> mu_1; //�ω��_�O�̕���
  real<lower = -100, upper = 100> mu_2; //�ω��_��̕���
  real<lower = 0> sigma;                //�W���΍�
}

transformed parameters {
  vector[n] lp;           // �eCP�i�ω��_�j�̑ΐ��ޓx���i�[����ꏊ
  lp = rep_vector(0, n);  // �ŏ���0�����񂾃x�N�g�����i�[���Ă���
  for (cp in 1:n){        // for�����g���āC1����123�܂őS�Ă�cp������
    for (t in 1:n){       // for�����g���āC�����cp�ɂ�����e���_(t)������
      if(t <= cp){        // cp�ȑO�̎��_�̏ꍇ
        if(t == 1){       // CP�ȑO�����_��1�̏ꍇ�̑ΐ��ޓx�̌v�Z
          lp[cp] = lp[cp] + normal_lpdf(text[t] | mu_1, sigma);
        }else{
          // CP�ȑO�̏ꍇ�̑ΐ��ޓx�̌v�Z
          lp[cp] = lp[cp] + normal_lpdf(text[t] | text[t-1] + mu_1, sigma);        
        }
      }else{
          // CP����̏ꍇ�̑ΐ��ޓx�̌v�Z  
          lp[cp] = lp[cp] + normal_lpdf(text[t] | text[t-1] + mu_2, sigma);
      }
    }
  }
}

model {
  //�Scp�ɂ�����ΐ��ޓx���w���ϊ�������Řa���Ƃ��đΐ��ϊ�
  target += log_sum_exp(lp); 
}

generated quantities{
  // �����ʂŗ��U�p�����[�^CP���T���v�����O
  int <lower = 1, upper = n> cp_s; //�T���v�����O���ꂽcp�@
  //categorical_logit_rng()�ŁC�eCP�̑ΐ��ޓx�̃x�N�g������cp���T���v�����O
  cp_s = categorical_logit_rng(lp); 
}
