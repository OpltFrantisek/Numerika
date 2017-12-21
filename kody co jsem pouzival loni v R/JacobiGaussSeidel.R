GS<-function(A,b){
  n<-nrow(A)
  x<-array(0,n)
  for(i in 1:9){
    for(j in 1:n){
      x[j]<-b[j]
      for(k in 1:n){
        if(k!=j) x[j]<-x[j]-A[j,k]*x[k]
      }
      x[j]<-x[j]/A[j,j]
    }
    cat(i)
    cat(format(x))
    cat("\n")
  }
  return(x)
}
Jacobi<-function(A,b){
  n<-nrow(A)
  x0<-array(0,n)
  x1<-x0
  for(i in 1:23){
    for(j in 1:n){
      x1[j]<-b[j]
      for(k in 1:n){
        if(k==j) next
        x1[j]<-x1[j]-A[j,k]*x0[k]
      }
      x1[j]<-x1[j]/A[j,j]
    }
    cat(i)
    cat(format(x1))
    cat("\n")
    x0<-x1
  }
  return(x1)
}
options(digits=8)
A<-rbind(c(10,-1,2,0),c(-1,11,-1,3),c(2,-1,10,-1),c(0,3,-1,8))
b<-c(6,25,-11,15)
cat(GS(A,b))
cat("\n")
cat(Jacobi(A,b))