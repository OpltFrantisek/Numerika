A<-rbind(c(-10,+1,2,0),c(-1,11,-1,5),c(2,-1,10,-1),c(0,3,-1,8))
n<-4
L<-matrix(0,n,n)
U<-matrix(0,n,n)
for(i in 1:n){
  L[i,i]=1
  U[1,i]=A[1,i]
  L[i,1]=A[i,1]/U[1,1]
}
for(r in 2:n){
  for(j in r:n){
    U[r,j]<-A[r,j]
    for(k in 1:(r-1)) U[r,j]<-U[r,j]-L[r,k]*U[k,j]
  }
  if(r<n){
    for(i in (r+1):n){
      L[i,r]<-A[i,r]
      for(k in 1:(r-1)) L[i,r]<-L[i,r]-L[i,k]*U[k,r]
      L[i,r]<-L[i,r]/U[r,r]
    }
  }
}
b<-c(5,7,-12,-14)
y<-b
for(i in 2:n){
  for(j in 1:(i-1)) y[i]<-y[i]-L[i,j]*y[j]
}
x<-y
x[n]<-x[n]/U[n,n]
for(i in (n-1):1){
  for(j in (i+1):n) x[i]<-x[i]-U[i,j]*x[j]
  x[i]<-x[i]/U[i,i]
}
print(x)
print(solve(A,b))