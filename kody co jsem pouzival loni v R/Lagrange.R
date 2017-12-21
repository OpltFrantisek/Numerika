Horner<-function(a,x){
  n<-length(a)
  y<-a[n]
  for(i in (n-1):1){
    y<-y*x+a[i]
  }
  return(y)
}
Newton<-function(x,y){
  n<-length(x)
  c<-y
  for(k in 2:n){
    f<-1
    P<-c[1]
    for(i in 1:(k-1)){
      f<-f*(x[k]-x[i])
      if(i<k-1) P<-P+c[i+1]*f
    }
    c[k]<-(c[k]-P)/f
  }
  return(c)
}
HornerN<-function(c,x,x0){
  n<-length(a)
  y<-c[n]
  for(i in (n-1):1){
    y<-y*(x0-x[i])+c[i]
  }
  return(y)
}
Lagrange<-function(x,y,x0){
  n<-length(x)
  sum<-0
  for(i in 1:n){
    l<-1
    for(j in 1:n){
      if(i!=j) l<-l*(x0-x[j])/(x[i]-x[j])
    }
    sum<-sum+l*y[i]
  }
  return(sum)
}
x<-c(1,1.5,2,3,5,5.5)
y<-c(10,8,3,5,6,7)
n<-length(x)
A<-matrix(0,n,n)
A[,1]<-array(1,n)
for(i in 1:n){
  for(j in 2:n){
    A[i,j]<-A[i,j-1]*x[i]
  }
}
a<-solve(A,y)
xa<-seq(min(x),max(x),(max(x)-min(x))/1000)
ya<-Horner(a,xa)
plot(xa,ya,type="l")
ya<-Lagrange(x,y,xa)
lines(xa,ya,col=3)
c<-Newton(x,y)
ya<-HornerN(c,x,xa)
lines(xa,ya,col=5)
points(x,y,col=2)