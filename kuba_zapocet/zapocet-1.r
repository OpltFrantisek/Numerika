linearFunc <- function(x,a,b){
    return(a*x+b)
}
GS<-function(A,b){
  n<-nrow(A)
  x<-array(0,n)
  for(i in 1:n){
    for(j in 1:n){
      x[j]<-b[j]
      for(k in 1:n){
        if(k!=j) x[j]<-x[j]-A[j,k]*x[k]
      }
      x[j]<-x[j]/A[j,j]
    }
    #cat(i)
    #cat(format(x))
    #cat("\n")
  }
  return(x)
}
GS2<-function(A,b){
  n<-nrow(A)
  for(k in 1:(n-1)){
    for(i in (k+1):n){
      koef<--A[i,k]/A[k,k]
      for(j in k:n){
        A[i,j]<-A[i,j]+koef*A[k,j]
      }
      b[i]<-b[i]+koef*b[k]
    }
  }
  x[n]<-b[n]/A[n,n]
  for(i in (n-1):1){
    x[i]<-(b[i]-sum(A[i,(i+1):n]*x[(i+1):n]))/A[i,i]
  }
  return(c(x[1],x[2]))
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
    cat("\n")
    print(nx)
    print(nxx)
    print(nxy)
    print(ny)
    cat("\n")
    A = matrix(c(nxx,nx,nx,n),nrow=2,ncol=2)
    b = c(nxy,ny)
    ab = GS2(A,b)
    print(ab)
    h = (x[n] -  x[1])/100
    xx = seq(x[1],x[n],h)

    yy = linearFunc(xx,ab[1],ab[2])
    plot(x,y)
    lines(xx,yy,col="blue")
    return(ab)
}
x<-c(0,1,2,3)
y<-c(-500,150,200,240)
a<-MNC(x,y)


print(a)
max <- 0

sumaCtvercu <- 0
for(i in 1:4){
    sumaCtvercu<- sumaCtvercu + (y[i] -linearFunc(x[i],a[1],a[2]))^2
}

for(i in 1:4){
    od <- abs(y[i]-linearFunc(x[i],a[1],a[2]))

    if( max < od ){
        max <- od
    }

}
print(max)
print(sumaCtvercu)
a[1] = a[1]
a[2] = a[2]*0.95
cat("\n")
print(a[1])
print(a[2])
cat("\n")
max <- 0
sumaCtvercu <- 0
for(i in 1:4){
    sumaCtvercu<- sumaCtvercu + (y[i] -linearFunc(x[i],a[1],a[2]))^2
}

for(i in 1:4){
    od <- abs(y[i]-linearFunc(x[i],a[1],a[2]))
    if( max < od ){
        max <- od
      }

}
    print(max)
    print(sumaCtvercu)


