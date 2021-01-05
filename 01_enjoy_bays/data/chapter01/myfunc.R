#MAP����l��^����֐�
MAP<-function(ext,dig=3){round(density(ext)$x[which.max(density(ext)$y)],dig)}

#a1��a2���seq01�傫���Ƃ��������������������Ȑ��̕`��
#a1��a2�͎��㕪�z�̗���
#seq01�͕`��͈̔́Aa2=0�Ȃ�a1��seq01���傫���m���̋Ȑ��ƂȂ�
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

#��������m���iext�̗�Ԃ�c�ȏ㍷������m���̍s����v�Z�j
#ext�͎��㕪�z�̗���
phc02<-function(ext,c=0, digits=5){
J<-ncol(ext)
pro_matrix<-matrix(0,J,J)
for (i in 1:J){  for (j in 1:J){
    pro_matrix[i,j]<-mean(ext[,i]-ext[,j]>c)  }}
round(pro_matrix,digits)
}

#�����ʂ̗v�񓝌v��(���o���t��)
#ext�͎��㕪�z�̗���
gen_quan01<-function(ext,dig=3,prob=c(0.025, 0.5, 0.975)){
a<-c(mean(ext), sd(ext),as.vector(quantile(ext,probs=prob)))
names(a)<-c("mean","post_sd",prob)
round(a,dig)
}

#�����ʂ̗v�񓝌v��(�S�_�v��)
#ext�͎��㕪�z�̗���
gen_quan02<-function(ext,dig=3){
cat(round(mean(ext),dig),"(",round(sd(ext),dig),")[",
    round(as.vector(quantile(ext,probs=0.025)),dig),",",
    round(as.vector(quantile(ext,probs=0.975)),dig),"]\n")
}
