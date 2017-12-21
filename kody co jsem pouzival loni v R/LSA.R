Horner<-function(a,x){
  n<-length(a)
  y<-a[n]
  for(i in (n-1):1){
    y<-y*x+a[i]
  }
  return(y)
}
f<-function(x,n){
  y<-array(1,n)
  if(n>1){
    for(i in 2:n){
      y[i]<-y[i-1]*x
    }
  }
  return(y)
}

x<-seq(0,pi,pi/20)
y<-sin(x)
m<-length(x)
plot(x,y)

xa<-seq(0,pi,pi/100)

for(n in 2:4){
  A<-matrix(0,n,m)
  S<-matrix(0,n,n)
  T<-array(0,n)
  for(j in 1:m){
    A[,j]<-f(x[j],n)
  }
  for(i in 1:n){
    for(j in 1:n){
      S[i,j]<-sum(A[i,]*A[j,])
    }
    T[i]<-sum(A[i,]*y)
  }
  a<-solve(S,T)
  #print(a)
  ya<-Horner(a,xa)
  line(xa,ya)
  #print(length(xa))
  #print(length(ya))
}