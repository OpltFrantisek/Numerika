f<-function(x){
  return(2*exp(-x*x)/sqrt(pi))
}
TrapezoidalRule<-function(f,a,b,n=1){
  h<-(b-a)/n
  sum<-(f(a)+f(b))/2
  x<-a
  if(n>1){
    for(i in 1:(n-1)){
      x<-x+h
      sum<-sum+f(x)
    } 
  }
  return(sum*h)
}
n<-10
R<-array(0,n)
for(i in 1:n) R[i]<-TrapezoidalRule(f,0,1,2^(i-1))
fac1<-1
for(j in 2:n){
  fac1<-fac1*4
  fac2<-1/(fac1-1)
  for(i in 1:(n-j+1)) R[i]<-fac2*(fac1*R[i+1]-R[i])
}
print(R[1])