主要な環境：R 3.4.4 　brms 2.4.0  TreeBUGS 1.4.0


チャプター12ではTreeBUGSパッケージにおいて
JAGSと呼ばれる外部プログラムを呼び出してMCMCを実行します。

1．以下のリンクから事前にJAGS をインストールしてください。
http://mcmc-jags.sourceforge.net/

2．事前にコンソール画面で以下のコマンドを実行してください。
install.packages("dplyr")
install.packages("tidyverse")
install.packages("brms")
install.packages("TreeBUGS")


Rchord.Rの前半ではシンプルな階層的信号検出理論の結果を
　sdt_Tai
で出力できます。

階層性を持ったSDTモデルとの結果（fitF）と比較してみてください。
本誌に載っている結果はfitF, fitRBから確認可能です。

また、SDTに関しては反応バイアスを算出するためのStanコード（SDT.stan）を作成しておきました。
Stanで見たい人は参考にしてください（事前分布の設定などはbrmsのデフォルトと対応しています。



MPTモデルの結果は1高閾モデルと2高閾モデルの比較を行っています。
PPP関数を用いた2モデルの比較ではあまり違いがないです。

本誌に載っている2高閾モデルは
　fitH2MPT
で出力できます。

TreeBUGSに関する詳細は以下のリンクから確認してみてください。
https://cran.r-project.org/web/packages/TreeBUGS/vignettes/TreeBUGS_1_intro.html
https://www.ncbi.nlm.nih.gov/pubmed/28374146

よい旅を！


難波修史