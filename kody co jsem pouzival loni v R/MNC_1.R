Horner<-function(a,x){
  n<-length(a)
  y<-a[n]
  for(i in (n-1):1){
    y<-y*x+a[i]
  }
  return(y)
}

koef<-1.5
x<-seq(0,koef*pi,koef*pi/100000)
y<-sin(x)
n<-length(x)
plot(x,y,type="l")

xa<-seq(0,koef*pi,koef*pi/100)

for(m in 2:4){
  start.time<-Sys.time()
  mm<-m+m-2
  mm1<-mm+1
  X<-matrix(1,n,mm1)
  for(j in 2:mm1){
    X[,j]<-X[,j-1]*x
  }
  s<-array(0,mm1)
  for(j in 1:mm1){
    s[j]<-sum(X[,j])
  }
  
  S<-matrix(0,m,m)
  T<-array(0,m)
  for(i in 1:m){
    for(j in 1:m){
      S[i,j]<-s[i+j-1]
    }
    T[i]<-T[i]+sum(y*X[,i])
  }
  a<-solve(S,T)
  print(a)
  end.time<-Sys.time()
  print(end.time-start.time)
  ya<-Horner(a,xa)
  lines(xa,ya,col=1+m,lw=2)
}