#http://sci.muny.cz/data/M4180/M4180-nm1.pdf, str. 10
NewtonHorner<-function(a){
  n<-length(a)
  x<-100
  repeat{
    b<-a[1]
    c<-b
    for(i in 2:(n-1)){
      b<-b*x+a[i]
      c<-c*x+b
    }
    b<-b*x+a[n]
    dx<--b/c
    x<-x+dx
    if(dx==0) break
  }
  return(x)
}
HornerPolDivision<-function(a,x0){
  n<-length(a)
  b<-a[1:(n-1)]
  for(i in 2:(n-1)){
      b[i]<-b[i-1]*x0+a[i]
  }
  return(b)
}

#(x-3)*(x+2)*(x-5)*(x+8)
a<-c(1,2,-49,22,240)
x0<-NewtonHorner(a)
print(x0)
a<-HornerPolDivision(a,x0)
#print(a)

x0<-NewtonHorner(a)
print(x0)
a<-HornerPolDivision(a,x0)
#print(a)

x0<-NewtonHorner(a)
print(x0)
a<-HornerPolDivision(a,x0)
#print(a)

x0<-NewtonHorner(a)
print(x0)