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


x<-c(1,2,3.5,4,6,7,8.5,10)
y<-c(10,7,5,1,4,12,3,2)
n<-length(x)
dx<-x[2:n]-x[1:(n-1)]
dy<-y[2:n]-y[1:(n-1)]

d<-(dy[2:(n-1)]/dx[2:(n-1)]-dy[1:(n-2)]/dx[1:(n-2)])
a<-(dx[2:(n-1)]+dx[1:(n-2)])/3
b<-dx[2:(n-2)]/6
c<-b

M<-TriDiag(a,b,c,d)
M<-c(0,M,0)
B<-y[1:(n-1)]-M[1:(n-1)]*dx*dx/6
A<-dy/dx+(M[1:(n-1)]-M[2:n])*dx/6
plot(x,y)

for(i in 1:(n-1)){
  xa<-seq(x[i],x[i+1],dx[i]/10)
  ya<-array(1,length(xa))
  for(j in 1:length(xa)){
    ya[j]<--M[i]*(xa[j]-x[i+1])^3/(6*dx[i])+M[i+1]*(xa[j]-x[i])^3/(6*dx[i])+A[i]*(xa[j]-x[i])+B[i]
  }
  lines(xa,ya,col="blue")
}


N = 100
I = 0
for(i in 1:(n-1)){
    h = (x[i+1]-x[i])/N
    xa<-seq(x[i],x[i+1],h)
    a = 0
    b = 0
    y1 =abs( M[i]*(xa[1]-x[i+1])^3/(6*dx[i])+M[i+1]*(xa[1]-x[i])^3/(6*dx[i])+A[i]*(xa[1]-x[i])+B[i])
    yn1 = abs(M[i]*(xa[n+1]-x[i+1])^3/(6*dx[i])+M[i+1]*(xa[n+1]-x[i])^3/(6*dx[i])+A[i]*(xa[n+1]-x[i])+B[i])
    for(j in 2:N){
        yn <- M[i]*(xa[j]-x[i+1])^3/(6*dx[i])+M[i+1]*(xa[j]-x[i])^3/(6*dx[i])+A[i]*(xa[j]-x[i])+B[i]
        if(j%%2 > 0){
            a = a+abs(yn)
        }
        else{
            b = b +abs(yn)
        }
    }
    a = 4*a
    b = 2*b
    I = I + (1/3)*((x[i+1] - x[i])/N)*(y1+a+b+yn1)
}
