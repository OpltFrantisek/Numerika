linearFunc <- function(x,a,b){
    return(a*x+b)
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
MNC <- function(x,y){
    n = length(x)
    nx <- 0 #suma x(i) od 1 do n
    nxx <- 0 #suma x(i)^2 od 1 do n
    nxy <- 0 #suma x(i)*y(i) od 1 do n
    ny <- 0 #suma y(i) od 1 do n
    for( i in 1:n){
        nx <- nx +x[i]
        nxx <- nxx + x[i]*x[i]
        nxy <- nxy + x[i]*y[i]
        ny <- ny + y[i]
    }
    A = matrix(c(nxx,nx,nx,n),nrow=2,ncol=2)
    b = c(nxy,ny)
    ab = GS(A,b)

    h = (x[n] -  x[1])/100
    xx = seq(x[1],x[n],h)
    yy = linearFunc(xx,ab[1],ab[2])
    plot(x,y)
    lines(xx,yy,col="blue")
}