___________________________Tugas 1 TEKSIM____________________________________

Fellya Naza Nurcahyani/B2A020054

multiplicative_RNG<-function(a,z0,m,n) {
  xf<-matrix(NA,n,3)
  colnames(xf)<-c("aZ","Xf","Uf")
  for (f in 1:n)
  {
    xf[f,1]<-(a*z0)
    xf[f,2]<-xf[f,1]%%m
    xf[f,3]<-xf[f,2]/m
    z0<-xf[f,2]
  }
  hist(xf[,3])
  View(xf)
}

Bernouli_2<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-(X<=p)+0
  (tabel<-table(Y)/length(Y))
}
#barplot(tabel,main="Bernoulli")