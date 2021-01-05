data {
  int playerNum;
  int hantyanNum;
  simplex [playerNum]point[hantyanNum];
}

parameters {
  vector <lower=0> [playerNum] janryoku;
  }

model {
  // ���̓x�N�g�����p�����[�^�Ƃ����f�B���N�����z����e�����̃f�[�^�𐶐�
  for(t in 1:hantyanNum){
    point[t,] ~ dirichlet(janryoku);
    }
}
