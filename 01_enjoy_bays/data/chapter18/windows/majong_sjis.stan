data {
  int playerNum;
  int hantyanNum;
  simplex [playerNum]point[hantyanNum];
}

parameters {
  vector <lower=0> [playerNum] janryoku;
  }

model {
  // 雀力ベクトルをパラメータとしたディリクレ分布から各半荘のデータを生成
  for(t in 1:hantyanNum){
    point[t,] ~ dirichlet(janryoku);
    }
}
