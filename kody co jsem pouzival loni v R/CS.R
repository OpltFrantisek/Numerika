TriDiag<-function(a,b,c,d){
  n<-length(a)
  u<-array(0,n)
  y<-u
  x<-y
  
  u[1]<-a[1]
  y[1]<-d[1]
  for(i in 2:n){
    koef<--b[i-1]/u[i-1]
    u[i]<-a[i]+koef*c[i-1]
    y[i]<-d[i]+koef*y[i-1]
  }
  x[n]<-y[n]/u[n]
  for(i in (n-1):1){
    x[i]<-(y[i]-c[i]*x[i+1])/u[i]
  }
  return(x)
}
Horner<-function(a,b,c,d,x,x0){
  dx<-x0-x
  return(((d*dx+c)*dx+b)*dx+a)
}
x<-c(1,2,3.5,4,6,7,8.5,10)
y<-c(10,7,5,1,4,12,3,2)
n<-length(x)
dx<-x[2:n]-x[1:(n-1)]
dy<-y[2:n]-y[1:(n-1)]
dd<-3*(dy[2:(n-1)]/dx[2:(n-1)]-dy[1:(n-2)]/dx[1:(n-2)])
aa<-2*(dx[2:(n-1)]+dx[1:(n-2)])
bb<-dx[2:(n-2)]
cc<-bb
c<-TriDiag(aa,bb,cc,dd)
c<-c(0,c,0)
d<-(c[2:n]-c[1:(n-1)])/(3*dx)
b<-dy/dx-((d*dx)+c[1:(n-1)])*dx
plot(x,y)
for(i in 1:(n-1)){
  xa<-seq(x[i],x[i+1],dx[i]/100)
  ya<-Horner(y[i],b[i],c[i],d[i],x[i],xa)
  lines(xa,ya,col="blue")
}