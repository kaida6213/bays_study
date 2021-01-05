data{
 int<lower=1> N; 
 int<lower=1> I; 
 int<lower=1> P; 
 int<lower=1> G; 
 int<lower=1> Gid[N];
 row_vector<lower=0>[P] Y[N];
}

parameters{
  vector[(I-4)] raw_lambda[2];      //����̂Ȃ����W�p�����[�^
  real<lower= 0,upper= 5> fix_x1;   //���ی��Ɍ���
  real<lower= 0,upper= 5> fix_y1;
  real<lower=-5,upper= 0> fix_x2;   //���ی��Ɍ���
  real<lower= 0,upper= 5> fix_y2;
  real<lower=-5,upper= 0> fix_x3;   //��O�ی��Ɍ���
  real<lower=-5,upper= 0> fix_y3;
  simplex[G] w0[2];
  real<lower=0> sig;
}

transformed parameters{
  vector[G] w[2];
  vector[I] const_lambda[2];      //�������ꂽ�p�����[�^�Z�b�g
  vector[I] lambda[2];            //�ŏI�I�ɐ��肷����W�Z�b�g

  const_lambda[1,1] = fix_x1;     //���ی��Ɍ���
  const_lambda[2,1] = fix_y1;
  const_lambda[1,2] = fix_x2;     //���ی��Ɍ���
  const_lambda[2,2] = fix_y2;
  const_lambda[1,3] = fix_x3;     //��O�ی��Ɍ���
  const_lambda[2,3] = fix_y3;
  const_lambda[1,4:(I-1)] = raw_lambda[1,];   //����ȊO�͐���̂Ȃ��p�����[�^����
  const_lambda[2,4:(I-1)] = raw_lambda[2,];
  const_lambda[1,I] = 0 - sum(const_lambda[1,1:(I-1)]); //���_���Œ�
  const_lambda[2,I] = 0 - sum(const_lambda[2,1:(I-1)]); //���_���Œ�
  
  lambda[1,] = const_lambda[1,]/(sqrt(dot_self(const_lambda[1,]))); //�m�����𐮂���
  lambda[2,] = const_lambda[2,]/(sqrt(dot_self(const_lambda[2,])));
  

  for(g in 1:G){
    for(j in 1:2){
      w[j,g] = w0[j,g]*G;
    }
  }

}

model{
  row_vector[P] d[N]; 

  for(n in 1:N){
    int pos=0;
    for(p in 1:(I-1)){
      for(q in (p+1):I){
        pos = pos +1;
        d[n,pos] = 0;
        for(j in 1:2){
          d[n,pos] = d[n,pos] + w[j,Gid[n]]*(lambda[j,p]-lambda[j,q])^2;
        }
        d[n,pos] = sqrt(d[n,pos]);
      }
    }
    Y[n] ~ normal(d[n],sig);
  }

  //prior
  for(j in 1:2){
    for(i in 1:(I-4)){
      raw_lambda[j,i] ~ normal(0,1);
    }
  }
  
  fix_x1 ~ normal(0,1);
  fix_y1 ~ normal(0,1);
  fix_x2 ~ normal(0,1);
  fix_y2 ~ normal(0,1);
  fix_x3 ~ normal(0,1);
  fix_y3 ~ normal(0,1);

  sig ~ student_t(4,0,5);

}
