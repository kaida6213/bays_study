#########################################################
##
## 第19章 著者: 北條大樹
## 作成日: 20180830
## Mail: dhojo.bayes@gmail.com
## HP: https://dastatis.github.io/index.html
## Researchmap: https://researchmap.jp/dastatis/
##
#########################################################

This "Readme.txt" is written by "ANSI".

はじめに...
本章をお読みくださってありがとうございます。
本章のRコードを動かすためには、各OSフォルダ内のRスクリプトのディレクトリをワーキングディレクトリとして設定してください。

・Rstudioをお使いの場合、
[files]タブからGUI操作で行うことができます。

・Rをお使いのかた、もしくはコマンド入力したい方
setwd("ファイルのパス")
と入力することで設定可能です。


コードについて
コメントアウトした部分が文字化けしないためにも以下の該当するコードをお使いください。

・Windows(CP932)でRをお使いの方
_winとついているコードをお使いください

・Mac(UTF-8)でRをお使いの方
_macとついているコードをお使いください

Narushi.Rが分析と作図用のコードになっています。

##### 重要 #####
最初にコードを実行するときのみ、Rスクリプトの以下の部分をコメントアウトして実行することで、
ワーキングディレクトリ内に分析データがダウンロードされます。

url <- "http://openpsychometrics.org/_rawdata/NPI.zip"
filename <- "NPI.zip"
download.file(url,filename,mode = "wb")
unzip("NPI.zip")


