#http://personalpages.manchester.ac.uk/staff/paul.connolly/teaching/practicals/solar_system.html

# plot(0,0,xlim=c(-1,1),ylim=c(-1,1))
# for(i in 1:360){
#   Sys.sleep(0.02)
#   uhel<-i*pi/180
#   x<-cos(uhel)
#   y<-sin(uhel)
#   points(x,y)
# }

#y<-c(x1,y1,x2,y2,x3,y3,vx1,vy1,...)
f<-function(x,y){
  kappa<-6.67*10^(-11)
  mass<-c(3.3022*10^23,4.8685*10^24,5.9736*10^24,6.4185*10^23,1.8986*10^27,5.6846*10^26,8.6810*10^25,10.243*10^25,1.9891*10^30)
  n<-9
  n2<-2*n
  n2m<-n2-1
  n2p<-n2+1
  n4<-4*n
  yder<-y
  yder[1:n2]<-y[n2p:n4]
  for(i in 1:(n-1)){
    ri<-y[(i*2-1):(i*2)] #(xi,yi)
    for(j in (i+1):n){
      rj<-y[(j*2-1):(j*2)] #(xj,yj)
      rij<-rj-ri
      rrij<-sum(rij*rij)
      a<-rij/(rrij*sqrt(rrij))
      yder[(i*2+n2m):(i*2+n2)]<-y[(i*2+n2m):(i*2+n2)]+kappa*mass[j]*a#ai
      yder[(j*2+n2m):(j*2+n2)]<-y[(j*2+n2m):(j*2+n2)]-kappa*mass[i]*a#aj
    }
  }
  return(yder)
}
RK1<-function(f,x,y,h){
  return(y+h*f(x,y))
}
z<-read.table("planets.txt",header=TRUE,sep=",")
n<-1000
dt<-1000
t<-dt*(0:(n-1))
y<-matrix(0,n,9*6)
for(i in 1:8){
  for(j in 1:3){
    y[1,3*i+j-3]<-z[i,j+1]
    y[1,3*i+j+24]<-z[i,j+4]
  }
}
for(i in 1:(n-1)){
  y[i+1,]<-RK1(f,t[i],y[i,],dt)
}