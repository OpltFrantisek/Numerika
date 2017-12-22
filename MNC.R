Horner<-function(a,x){
  n<-length(a)
  y<-a[n]
  for(i in (n-1):1){
    y<-y*x+a[i]
  }
  return(y)
}
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
MNC <- function(stupen,x,y){

    n <- length(x)
    m<-stupen+stupen-2
    m1<-m+1

    X<-matrix(1,n,m1) # matice s 1

   for(j in 2:m1){
        X[,j]<-X[,j-1]*x
   }
   s<-array(0,m1) # vektor s 0
   for(j in 1:m1){
       s[j]<-sum(X[,j])
   }
    S<-matrix(0,stupen,stupen)
    T<-array(0,stupen)
    for(i in 1:stupen){
        for(j in 1:stupen){
            S[i,j]<-s[i+j-1]
         }
        T[i]<-T[i]+sum(y*X[,i])
    }
    a<-GS(S,T)
    return(a)
}


x<-c(0,1,2,3)
y<-c(-500,150,200,240)
plot(x,y,type="b")
xa<-seq(0,90,0.01)
a<-MNC(2,x,y)
ya<-Horner(a,xa)
lines(xa,ya,col=4,lw=2)