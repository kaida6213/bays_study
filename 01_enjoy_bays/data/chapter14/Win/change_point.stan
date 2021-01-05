data {
  int n;         // データポイント数
  real text[n];  // 文字数
}

parameters {
  real<lower = -100, upper = 100> mu_1; //変化点前の平均
  real<lower = -100, upper = 100> mu_2; //変化点後の平均
  real<lower = 0> sigma;                //標準偏差
}

transformed parameters {
  vector[n] lp;           // 各CP（変化点）の対数尤度を格納する場所
  lp = rep_vector(0, n);  // 最初に0が並んだベクトルを格納しておく
  for (cp in 1:n){        // for文を使って，1から123まで全てのcpを検討
    for (t in 1:n){       // for文を使って，特定のcpにおける各時点(t)を検討
      if(t <= cp){        // cp以前の時点の場合
        if(t == 1){       // CP以前かつ時点が1の場合の対数尤度の計算
          lp[cp] = lp[cp] + normal_lpdf(text[t] | mu_1, sigma);
        }else{
          // CP以前の場合の対数尤度の計算
          lp[cp] = lp[cp] + normal_lpdf(text[t] | text[t-1] + mu_1, sigma);        
        }
      }else{
          // CPより後の場合の対数尤度の計算  
          lp[cp] = lp[cp] + normal_lpdf(text[t] | text[t-1] + mu_2, sigma);
      }
    }
  }
}

model {
  //全cpにおける対数尤度を指数変換した上で和をとって対数変換
  target += log_sum_exp(lp); 
}

generated quantities{
  // 生成量で離散パラメータCPをサンプリング
  int <lower = 1, upper = n> cp_s; //サンプリングされたcp　
  //categorical_logit_rng()で，各CPの対数尤度のベクトルからcpをサンプリング
  cp_s = categorical_logit_rng(lp); 
}
