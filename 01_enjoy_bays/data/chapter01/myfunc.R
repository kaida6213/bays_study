#MAP推定値を与える関数
MAP<-function(ext,dig=3){round(density(ext)$x[which.max(density(ext)$y)],dig)}

#a1がa2よりseq01大きいという研究仮説が正しい曲線の描画
#a1とa2は事後分布の乱数
#seq01は描画の範囲、a2=0ならa1がseq01より大きい確率の曲線となる
phc01<-function(seq01,a1,a2,xlab="x",ceax=2.0){
  a<-numeric(length(seq01))
  j<-0
  for(c in seq01){ 
    j<-j+1; 
    a[j]<-mean(a1-a2>c);}
  plot(seq01,a,lwd=2,type="l",ylab="",
    xlab=xlab,xlim=range(seq01),cex.axis=ceax)
  grid(lwd=2.0)
}

#差がある確率（extの列間にc以上差がある確率の行列を計算）
#extは事後分布の乱数
phc02<-function(ext,c=0, digits=5){
J<-ncol(ext)
pro_matrix<-matrix(0,J,J)
for (i in 1:J){  for (j in 1:J){
    pro_matrix[i,j]<-mean(ext[,i]-ext[,j]>c)  }}
round(pro_matrix,digits)
}

#生成量の要約統計量(見出し付き)
#extは事後分布の乱数
gen_quan01<-function(ext,dig=3,prob=c(0.025, 0.5, 0.975)){
a<-c(mean(ext), sd(ext),as.vector(quantile(ext,probs=prob)))
names(a)<-c("mean","post_sd",prob)
round(a,dig)
}

#生成量の要約統計量(４点要約)
#extは事後分布の乱数
gen_quan02<-function(ext,dig=3){
cat(round(mean(ext),dig),"(",round(sd(ext),dig),")[",
    round(as.vector(quantile(ext,probs=0.025)),dig),",",
    round(as.vector(quantile(ext,probs=0.975)),dig),"]\n")
}

