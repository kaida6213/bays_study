執筆量モニタリングにおける変化点検出のRコード

専修大学人間科学部心理学科　国里愛彦

本フォルダには，執筆量モニタリングにおける変化点検出で用いた以下のデータとRコードが含まれています。
・change_point.R (メインのRコード)
・change_point.stan (stanコード)
・writing_analysis_function.R（MCMCの実行と推定結果を出力する自作関数）
・data.csv（執筆量データ）
・draft_info.csv (原稿データ)

Rコードの実行方法
本フォルダをカレントワーキングディレクトリーに設定した上で，change_point.Rを上から順番に実行していくと教科書と同じ結果が再現されるかと思います。change_point.stanとwriting_analysis_function.Rは，change_point.Rを上から順番に実行していくと適宜読み込まれます。なお，高速化のための工夫をしていないため，環境によっては各データに対する推定には時間がかかります。予めご了承ください。

参考文献
・Lee, M. D. and Wagenmakers, E. J. 2013 Bayesian cognitive modeling: a practical course. Cambridge, UK: Cambridge University press. 井関 龍太 (訳)・岡田 謙介（解説）2017 ベイズ統計で実践モデリング: 認知モデルのトレーニング 北大路書房
・Stan Development Team (2017) 15.Latent Discrete Parameters. Stan Modeling Language: User’s Guide and Reference Manual. Version 2.17.0.
・松浦健太郎 2016 StanとRでベイズ統計モデリング　共立出版
・松浦健太郎氏のブログ記事 (http://statmodeling.hatenablog.com/entry/cumulative_sum-to-reduce-calculation)

問い合わせ先
Rコードに関して不明な点や誤りなどがありましたら，国里(ykunisato@psy.senshu-u.ac.jp)までお問い合わせください。