f1<-function(x){return(exp(x)-1-x)}
f2<-function(x){return((exp(x)+exp(1-x))/2)}
fIntStrHod<-function(f,n,a,b){
  sum<-0
  for(i in 1:n){
    sum<-sum+f(runif(1,a,b))
  }
  return(sum*(b-a)/n)
}
fIntStrHodVazVyb<-function(n,a,b){
  sum<-0
  for(i in 1:n){
    x<-sqrt(1+3*runif(1,a,b))-1
    sum<-sum+exp(x)/(1+x)
  }
  return(1.5*sum*(b-a)/n)
}
fIntGeom<-function(f,n,a,b,h){
  sum<-0
  for(i in 1:n){
    if(runif(1,0,h)<f(runif(1,a,b))) sum<-sum+1
  }
  return((b-a)*h*sum/n)
}
n<-100
m<-1000
xvec<-array(0,m)

for(i in 1:m) xvec[i]<-fIntStrHod(exp,n,0,1)
print(mean(xvec))
print(var(xvec))

for(i in 1:m) xvec[i]<-fIntGeom(exp,n,0,1,exp(1))
print(mean(xvec))
print(var(xvec))

for(i in 1:m) xvec[i]<-fIntStrHod(f1,n,0,1)+1.5 #nalezeni hlavni casti
print(mean(xvec))
print(var(xvec))

for(i in 1:m) xvec[i]<-fIntStrHod(f2,n,0,1) #symetrizace
print(mean(xvec))
print(var(xvec))

for(i in 1:m) xvec[i]<-fIntStrHodVazVyb(n,0,1)
print(mean(xvec))
print(var(xvec))